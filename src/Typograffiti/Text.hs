{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
-- |
-- Module:     Typograffiti.Monad
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- Text rendering abstraction, hiding the details of
-- the Atlas, Cache, and the Harfbuzz library.
module Typograffiti.Text where


import           Control.Monad.Except   (MonadError (..), runExceptT)
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad          (foldM, forM, unless)
import qualified Data.IntSet            as IS
import           Linear                 (V2 (..))
import qualified Data.ByteString        as B
import           Data.Text.Glyphize     (defaultBuffer, shape, GlyphInfo (..),
                                        parseFeature, parseVariation, Variation (..),
                                        FontOptions (..), defaultFontOptions)
import qualified Data.Text.Glyphize     as HB
import           FreeType.Core.Base
import           FreeType.Core.Types    (FT_Fixed)
import           FreeType.Format.Multiple (ft_Set_Var_Design_Coordinates)
import           Data.Text.Lazy         (Text, pack)
import qualified Data.Text.Lazy         as Txt
import           Data.Word              (Word32)

import           Typograffiti.Atlas
import           Typograffiti.Cache
import           Typograffiti.Rich      (RichText(..))

-- | How large the text should be rendered.
data GlyphSize = CharSize Float Float Int Int
                -- ^ Size in Pts at given DPI.
               | PixelSize Int Int
               -- ^ Size in device pixels.
               deriving (Show, Eq, Ord)

-- | Extra parameters for constructing a font atlas,
-- and determining which glyphs should be in it.
data SampleText = SampleText {
    sampleFeatures :: [HB.Feature],
    -- ^ Which OpenType Features you want available to be used in the rendered text.
    -- Defaults to none.
    sampleText :: Text,
    -- ^ Indicates which characters & ligatures will be in the text to be rendered.
    -- Defaults to ASCII, no ligatures.
    tabwidth :: Int,
    -- ^ How many spaces wide should a tab be rendered?
    -- Defaults to 4.
    fontOptions :: FontOptions
    -- ^ Additional font options offered by Harfbuzz.
}

-- | Constructs a `SampleText` with default values.
defaultSample :: SampleText
defaultSample = SampleText [] (pack $ map toEnum [32..126]) 4 defaultFontOptions
-- | Appends an OpenType feature callers may use to the `Sample` ensuring its
-- glyphs are available. Call after setting `sampleText`.
addSampleFeature :: String -> Word32 -> SampleText -> SampleText
addSampleFeature name value sample@SampleText {..} = sample {
        sampleFeatures =
            HB.Feature (HB.tag_from_string name) value (n*i) (n*succ i) : sampleFeatures
    }
  where
    n = w $ fromEnum $ Txt.length sampleText
    i = w $ length sampleFeatures
    w :: Int -> Word
    w = toEnum
-- | Parse an OpenType feature into this font using syntax akin to
-- CSS font-feature-settings.
parseSampleFeature :: String -> SampleText -> SampleText
parseSampleFeature syntax sample | Just feat <- parseFeature syntax = sample {
        sampleFeatures = feat : sampleFeatures sample
    }
  | otherwise = sample
-- | Parse multiple OpenType features into this font.
parseSampleFeatures :: [String] -> SampleText -> SampleText
parseSampleFeatures = flip $ foldl $ flip parseSampleFeature
-- | Alter which OpenType variant of this font will be rendered.
-- Please check your font which variants are supported.
addFontVariant :: String -> Float -> SampleText -> SampleText
addFontVariant name val sampleText = sampleText {
    fontOptions = (fontOptions sampleText) {
        optionVariations = Variation (HB.tag_from_string name) val :
            optionVariations (fontOptions sampleText)
    }
  }
-- | Parse a OpenType variant into the configured font using syntax akin to
-- CSS font-variant-settings.
parseFontVariant :: String -> SampleText -> SampleText
parseFontVariant syntax sample | Just var <- parseVariation syntax = sample {
        fontOptions = (fontOptions sample) {
            optionVariations = var : optionVariations (fontOptions sample)
        }
    }
  | otherwise = sample
-- | Parse multiple OpenType variants into this font.
parseFontVariants :: [String] -> SampleText -> SampleText
parseFontVariants = flip $ foldl $ flip parseFontVariant

-- | Standard italic font variant. Please check if your font supports this.
varItalic = "ital"
-- | Standard optical size font variant. Please check if your font supports this.
varOptSize = "opsz"
-- | Standard slant (oblique) font variant. Please check if your font supports this.
varSlant = "slnt"
-- | Standard width font variant. Please check if your font supports this.
varWidth = "wdth"
-- | Standard weight (boldness) font variant. Please check if your font supports this.
varWeight = "wght"

-- | Opens a font sized to the given value & prepare to render text in it.
-- There is no need to keep the given `FT_Library` live before rendering the text.
makeDrawText :: (MonadIO m, MonadFail m, MonadError TypograffitiError m,
    MonadIO n, MonadFail n, MonadError TypograffitiError n) =>
    FT_Library -> FilePath -> Int -> GlyphSize -> SampleText ->
    m (RichText -> n (AllocatedRendering [TextTransform]))
makeDrawText lib filepath index fontsize SampleText {..} = do
    font <- liftFreetype $ ft_New_Face lib filepath $ toEnum index
    liftFreetype $ case fontsize of
        PixelSize w h -> ft_Set_Pixel_Sizes font (toEnum $ x2 w) (toEnum $ x2 h)
        CharSize w h dpix dpiy -> ft_Set_Char_Size font (floor $ 26.6 * 2 * w)
                                                    (floor $ 26.6 * 2 * h)
                                                    (toEnum dpix) (toEnum dpiy)

    bytes <- liftIO $ B.readFile filepath
    let font' = HB.createFontWithOptions fontOptions $ HB.createFace bytes $ toEnum index
    let glyphs = map (codepoint . fst) $
            shape font' defaultBuffer {
                HB.text = Txt.replicate (toEnum $ succ $ length sampleFeatures) sampleText
            } sampleFeatures
    let glyphs' = map toEnum $ IS.toList $ IS.fromList $ map fromEnum glyphs

    let designCoords = map float2fixed $ HB.fontVarCoordsDesign font'
    unless (null designCoords) $
        liftFreetype $ ft_Set_Var_Design_Coordinates font designCoords

    atlas <- allocAtlas (glyphRetriever font) glyphs'
    liftFreetype $ ft_Done_Face font

    drawGlyphs <- makeDrawGlyphs
    return $ freeAtlasWrapper atlas $ drawLinesWrapper tabwidth $ \RichText {..} ->
        drawGlyphs atlas $ shape font' defaultBuffer { HB.text = text } features
  where
    x2 = (*2)
    float2fixed :: Float -> FT_Fixed
    float2fixed = toEnum . fromEnum . (*65536)

-- | Variant of `makeDrawText` which initializes FreeType itself.
makeDrawText' a b c d =
    ft_With_FreeType $ \ft -> runExceptT $ makeDrawText ft a b c d

-- | Internal utility for rendering multiple lines of text & expanding tabs as configured.
type TextRenderer m = RichText -> m (AllocatedRendering [TextTransform])
drawLinesWrapper :: (MonadIO m, MonadFail m) => Int -> TextRenderer m -> TextRenderer m
drawLinesWrapper indent cb RichText {..} = do
    let features' = splitFeatures 0 features (Txt.lines text) ++ repeat []
    let cb' (a, b) = cb $ RichText a b
    liftIO $ print $ Txt.lines text
    renderers <- mapM cb' $ flip zip features' $ map processLine $ Txt.lines text
    let drawLine ts wsz y renderer = do
            arDraw renderer (move 0 y:ts) wsz
            let V2 _ height = arSize renderer
            return (y + toEnum height)
    let draw ts wsz = do
            foldM (drawLine ts wsz) 0 renderers
            return ()
    let sizes = map arSize renderers
    let size = V2 (maximum [x | V2 x _ <- sizes]) (sum [y | V2 _ y <- sizes])
    let release = do
            forM renderers arRelease
            return ()
    return AllocatedRendering {
            arDraw = draw,
            arRelease = release,
            arSize = size
          }
  where
    splitFeatures :: Word -> [HB.Feature] -> [Text] -> [[HB.Feature]]
    splitFeatures _ [] _ = []
    splitFeatures _ _ [] = []
    splitFeatures offset features' (line:lines') = let n = fromEnum $ Txt.length line
        in [feat {
                HB.featStart = max 0 (start - offset),
                HB.featEnd = min (toEnum n) (end - offset)
              }
            | feat@HB.Feature {HB.featStart = start, HB.featEnd = end} <- features',
            fromEnum end <= n + fromEnum offset && end >= offset] :
            splitFeatures (offset + toEnum n) features' lines'

    processLine :: Text -> Text
    processLine = expandTabs 0
    -- monospace tabshaping, good enough outside full line-layout.
    expandTabs n cs = case Txt.break (== '\t') cs of
        (tail, "") -> tail
        (pre, cs') ->
            let spaces = indent - ((fromEnum (Txt.length pre) + fromEnum n) `rem` indent)
            in Txt.concat [pre, Txt.replicate (toEnum spaces) " ",
                expandTabs (n + Txt.length pre + toEnum spaces) $ Txt.tail cs']

freeAtlasWrapper :: MonadIO m => Atlas -> TextRenderer m -> TextRenderer m
freeAtlasWrapper atlas cb text = do
    ret <- cb text
    return ret {
        arRelease = do
            arRelease ret
            freeAtlas atlas
    }
