{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- |
-- Module:     Typograffiti.Monad
-- Copyright:  (c) 2018 Schell Scivally, 2023 Adrian Cochrane
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--             & Adrian Cochrane <alcinnz@argonaut-constellation.org>
--
-- A storage context an ops for rendering text with multiple fonts
-- and sizes, hiding the details of the Atlas, Cache, and the Harfbuzz library.
module Typograffiti.Store where


import           Control.Concurrent.STM (TMVar, atomically, newTMVar, putTMVar,
                                         readTMVar, takeTMVar)
import           Control.Monad.Except   (MonadError (..), runExceptT, ExceptT (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad          (unless, forM)
import           Data.Map               (Map)
import qualified Data.Map               as M
import qualified Data.IntSet            as IS
import qualified Data.ByteString        as B
import           Data.Text.Glyphize     (defaultBuffer, Buffer(..), shape,
                                        GlyphInfo(..), GlyphPos(..), FontOptions)
import qualified Data.Text.Glyphize     as HB
import qualified Data.Text.Lazy         as Txt
import           Foreign.Storable       (peek)
import           FreeType.Core.Base
import           FreeType.Core.Types    (FT_Fixed, FT_UShort)
import           FreeType.Format.Multiple (ft_Set_Var_Design_Coordinates)

import           Typograffiti.Atlas
import           Typograffiti.Cache
import           Typograffiti.Text      (GlyphSize(..), drawLinesWrapper, SampleText(..))
import           Typograffiti.Rich      (RichText(..))

-- | Stored fonts at specific sizes.
data FontStore n = FontStore {
    fontMap :: TMVar (Map (FilePath, GlyphSize, Int, FontOptions) Font),
    -- ^ Map for looking up previously-opened fonts & their atlases.
    drawGlyphs :: Atlas -> [(GlyphInfo, GlyphPos)] -> n (AllocatedRendering [TextTransform]),
    -- ^ Cached routine for compositing from the given atlas.
    lib :: FT_Library
    -- ^ Globals for FreeType.
  }
-- | An opened font. In Harfbuzz, FreeType, & Atlas formats.
data Font = Font {
    harfbuzz :: HB.Font,
    -- ^ Font as represented by Harfbuzz.
    freetype :: FT_Face,
    -- ^ Font as represented by FreeType.
    atlases :: TMVar [(IS.IntSet, Atlas)],
    -- ^ Glyphs from the font rendered into GPU atleses.
    lineHeight :: Float,
    -- ^ Default lineheight for this font.
    fontScale :: (Float, Float)
    -- ^ Scaling parameters for Harfbuzz layout.
  }

-- | Opens a font sized to given value & prepare to render text in it.
-- The fonts are cached for later reuse.
makeDrawTextCached :: (MonadIO m, MonadFail m, MonadError TypograffitiError m,
    MonadIO n, MonadFail n, MonadError TypograffitiError n) =>
    FontStore n -> FilePath -> Int -> GlyphSize -> SampleText ->
    m (RichText -> n (AllocatedRendering [TextTransform]))
makeDrawTextCached store filepath index fontsize SampleText {..} = do
    s <- liftIO $ atomically $ readTMVar $ fontMap store
    let fontOpts' = fontOptions {
        HB.optionScale = Nothing, HB.optionPtEm = Nothing, HB.optionPPEm = Nothing
      }
    font <- case M.lookup (filepath, fontsize, index, fontOpts') s of
        Nothing -> allocFont store filepath index fontsize fontOpts'
        Just font -> return font

    let glyphs = map (codepoint . fst) $
            shape (harfbuzz font) defaultBuffer {
                HB.text = Txt.replicate (toEnum $ succ $ length sampleFeatures) sampleText
            } sampleFeatures
    let glyphset = IS.fromList $ map fromEnum glyphs

    a <- liftIO $ atomically $ readTMVar $ atlases font
    atlas <- case [a' | (gs, a') <- a, glyphset `IS.isSubsetOf` gs] of
        (atlas:_) -> return atlas
        _ -> allocAtlas' (atlases font) (freetype font) glyphset (fontScale font)

    let lh = if minLineHeight == 0 then lineHeight font else minLineHeight
    return $ drawLinesWrapper tabwidth lh $
        \RichText {..} -> drawGlyphs store atlas $
            shape (harfbuzz font) defaultBuffer { HB.text = text } []

-- | Opens & sizes the given font using both FreeType & Harfbuzz,
-- loading it into the `FontStore` before returning.
allocFont :: (MonadIO m, MonadError TypograffitiError m) =>
        FontStore n -> FilePath -> Int -> GlyphSize -> HB.FontOptions -> m Font
allocFont FontStore {..} filepath index fontsize options = liftFreetype $ do
    font <- ft_New_Face lib filepath $ toEnum index
    case fontsize of
        PixelSize w h -> ft_Set_Pixel_Sizes font (toEnum $ x2 w) (toEnum $ x2 h)
        CharSize w h dpix dpiy -> ft_Set_Char_Size font (floor $ 26.6 * 2 * w)
                                                    (floor $ 26.6 * 2 * h)
                                                    (toEnum dpix) (toEnum dpiy)

    bytes <- B.readFile filepath
    let font' = HB.createFontWithOptions options $ HB.createFace bytes $ toEnum index

    let designCoords = map float2fixed $ HB.fontVarCoordsDesign font'
    unless (null designCoords) $ liftIO $ ft_Set_Var_Design_Coordinates font designCoords

    font_ <- liftIO $ peek font
    size <- srMetrics <$> liftIO (peek $ frSize font_)
    let lineHeight = fixed2float $ smHeight size
    let upem = short2float $ frUnits_per_EM font_
    let scale = (short2float (smX_ppem size)/upem/2, short2float (smY_ppem size)/upem/2)

    atlases <- liftIO $ atomically $ newTMVar []
    let ret = Font font' font atlases lineHeight scale

    atomically $ do
        map <- takeTMVar fontMap
        putTMVar fontMap $ M.insert (filepath, fontsize, index, options) ret map
    return ret
  where
    x2 = (*2)
    float2fixed :: Float -> FT_Fixed
    float2fixed = toEnum . fromEnum . (*bits16)
    fixed2float :: FT_Fixed -> Float
    fixed2float = (/bits16) . toEnum . fromEnum
    bits16 = 2**16
    short2float :: FT_UShort -> Float
    short2float = toEnum . fromEnum

-- | Allocates a new Atlas for the given font & glyphset,
-- loading it into the atlas cache before returning.
allocAtlas' :: (MonadIO m, MonadFail m, MonadError TypograffitiError m) =>
    TMVar [(IS.IntSet, Atlas)] -> FT_Face -> IS.IntSet -> (Float, Float) -> m Atlas
allocAtlas' atlases font glyphset scale = do
    let glyphs = map toEnum $ IS.toList glyphset
    atlas <- allocAtlas (glyphRetriever font) glyphs scale

    liftIO $ atomically $ do
        a <- takeTMVar atlases
        putTMVar atlases $ ((glyphset, atlas):a)
    return atlas

-- | Frees fonts identified by filepath, index, and\/or fontsize.
-- Returns the glyphsets covered by their newly-freed atlases in case
-- callers wish to make an informed reallocation.
freeFonts :: (MonadIO m, MonadError TypograffitiError m) =>
    FontStore n -> Maybe FilePath -> Maybe Int -> Maybe GlyphSize -> m IS.IntSet
freeFonts store filepath index size = do
    let test (filepath', size', index', _) = case (filepath, index, size) of
            (Just f, Just i, Just s) -> filepath' == f && index' == i && size' == s
            (Nothing,Just i, Just s) -> index' == i && size' == s
            (Just f, Nothing,Just s) -> filepath' == f && size' == s
            (Nothing,Nothing,Just s) -> size' == s
            (Just f, Just i, Nothing)-> filepath' == f && index' == i
            (Nothing,Just i, Nothing)-> index' == i
            (Just f, Nothing,Nothing)-> filepath' == f
            (Nothing,Nothing,Nothing)-> True
    fonts <- liftIO $ atomically $ do
        fonts <- readTMVar $ fontMap store
        putTMVar (fontMap store) $ M.filterWithKey (\k _ -> not $ test k) fonts
        return fonts

    glyphsets <- forM [v | (k, v) <- M.toList fonts, test k] $ \font -> do
        liftFreetype $ ft_Done_Face $ freetype font
        -- Harfbuzz font auto-frees.
        atlases' <- liftIO $ atomically $ readTMVar $ atlases font
        glyphsets <- forM atlases' $ \(glyphset, atlas) -> do
            freeAtlas atlas
            return glyphset
        return $ IS.unions glyphsets
    return $ IS.unions glyphsets

-- | Runs the given callback with a new `FontStore`.
-- Due to FreeType limitations this font store should not persist outside the callback.
withFontStore :: (MonadIO n, MonadError TypograffitiError n, MonadFail n) =>
    (FontStore n -> ExceptT TypograffitiError IO a) ->
    IO (Either TypograffitiError a)
withFontStore cb = ft_With_FreeType $ \lib -> runExceptT $ do
    store <- newFontStore lib
    ret <- cb store
    freeFonts store Nothing Nothing Nothing
    return ret

-- | Allocates a new FontStore wrapping given FreeType state.
newFontStore :: (MonadIO m, MonadError TypograffitiError m,
    MonadIO n, MonadError TypograffitiError n, MonadFail n) => FT_Library -> m (FontStore n)
newFontStore lib = do
    drawGlyphs <- makeDrawGlyphs
    store <- liftIO $ atomically $ newTMVar M.empty

    return $ FontStore store drawGlyphs lib
