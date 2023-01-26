{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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


import           Control.Concurrent.STM (TMVar, atomically, newTMVar, putTMVar,
                                         readTMVar, takeTMVar)
import           Control.Monad.Except   (MonadError (..), liftEither, runExceptT)
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad          (foldM, forM)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Set               (Set)
import qualified Data.Set               as S
import qualified Data.IntSet            as IS
import           Linear
import qualified Data.ByteString        as B
import           Data.Text.Glyphize     (defaultBuffer, Buffer(..), shape, GlyphInfo(..),
                                        parseFeature, parseVariation, Variation(..),
                                        FontOptions(..), defaultFontOptions)
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

data GlyphSize = CharSize Float Float Int Int
               | PixelSize Int Int
               deriving (Show, Eq, Ord)

data SampleText = SampleText {
    sampleFeatures :: [HB.Feature],
    sampleText :: Text,
    tabwidth :: Int,
    fontOptions :: FontOptions
}

defaultSample :: SampleText
defaultSample = SampleText [] (pack $ map toEnum [32..126]) 4 defaultFontOptions
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
parseSampleFeature :: String -> SampleText -> SampleText
parseSampleFeature syntax sample | Just feat <- parseFeature syntax = sample {
        sampleFeatures = feat : sampleFeatures sample
    }
  | otherwise = sample
parseSampleFeatures :: [String] -> SampleText -> SampleText
parseSampleFeatures = flip $ foldl $ flip parseSampleFeature
addFontVariant :: String -> Float -> SampleText -> SampleText
addFontVariant name val sampleText = sampleText {
    fontOptions = (fontOptions sampleText) {
        optionVariations = Variation (HB.tag_from_string name) val :
            optionVariations (fontOptions sampleText)
    }
  }
parseFontVariant :: String -> SampleText -> SampleText
parseFontVariant syntax sample | Just var <- parseVariation syntax = sample {
        fontOptions = (fontOptions sample) {
            optionVariations = var : optionVariations (fontOptions sample)
        }
    }
  | otherwise = sample
parseFontVariants :: [String] -> SampleText -> SampleText
parseFontVariants = flip $ foldl $ flip parseFontVariant

varItalic = "ital"
varOptSize = "opsz"
varSlant = "slnt"
varWidth = "wdth"
varWeight = "wght"

makeDrawText :: (MonadIO m, MonadFail m, MonadError TypograffitiError m,
    MonadIO n, MonadFail n, MonadError TypograffitiError n) =>
    FT_Library -> FilePath -> Int -> GlyphSize -> SampleText ->
    m (RichText -> n (AllocatedRendering [TextTransform]))
makeDrawText lib filepath index fontsize SampleText {..} = do
    font <- liftIO $ ft_New_Face lib filepath $ toEnum index
    liftIO $ case fontsize of
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
    -- FIXME expose this function...
    liftIO $ ft_Set_Var_Design_Coordinates font $ map float2fixed $ HB.fontVarCoordsDesign font'
    atlas <- allocAtlas (glyphRetriever font) glyphs'
    liftIO $ ft_Done_Face font

    drawGlyphs <- makeDrawGlyphs
    return $ drawLinesWrapper tabwidth $ \RichText {..} ->
        drawGlyphs atlas $ shape font' defaultBuffer { HB.text = text } features
  where
    x2 = (*2)
    float2fixed :: Float -> FT_Fixed
    float2fixed = toEnum . fromEnum . (*65536)

makeDrawText' a b c d =
    ft_With_FreeType $ \ft -> runExceptT $ makeDrawText ft a b c d

drawLinesWrapper :: (MonadIO m, MonadFail m) =>
    Int -> (RichText -> m (AllocatedRendering [TextTransform])) ->
    RichText -> m (AllocatedRendering [TextTransform])
drawLinesWrapper indent cb RichText {..} = do
    let features' = splitFeatures 0 features $ Txt.lines text
    let cb' (a, b) = cb $ RichText a b
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
    processLine "" = " " -- enforce nonempty
    processLine cs = expandTabs 0 cs
    -- monospace tabshaping, good enough outside full line-layout.
    expandTabs n cs = case Txt.break (== '\t') cs of
        (tail, "") -> tail
        (pre, cs') ->
            let spaces = indent - ((fromEnum (Txt.length pre) + fromEnum n) `rem` indent)
            in Txt.concat [pre, Txt.replicate (toEnum spaces) " ",
                expandTabs (n + Txt.length pre + toEnum spaces) $ Txt.tail cs']
