{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- |
-- Module:     Typograffiti.Monad
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- A storage context an ops for rendering text with multiple fonts
-- and sizes, hiding the details of the Atlas, Cache, and the Harfbuzz library.
module Typograffiti.Store where


import           Control.Concurrent.STM (TMVar, atomically, newTMVar, putTMVar,
                                         readTMVar, takeTMVar)
import           Control.Monad.Except   (MonadError (..), runExceptT, ExceptT (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad          (unless)
import           Data.Map               (Map)
import qualified Data.Map               as M
import qualified Data.IntSet            as IS
import qualified Data.ByteString        as B
import           Data.Text.Glyphize     (defaultBuffer, Buffer(..), shape,
                                        GlyphInfo(..), GlyphPos(..), FontOptions)
import qualified Data.Text.Glyphize     as HB
import qualified Data.Text.Lazy         as Txt
import           FreeType.Core.Base
import           FreeType.Core.Types    (FT_Fixed)
import           FreeType.Format.Multiple (ft_Set_Var_Design_Coordinates)

import           Typograffiti.Atlas
import           Typograffiti.Cache
import           Typograffiti.Text      (GlyphSize(..), drawLinesWrapper, SampleText(..))
import           Typograffiti.Rich      (RichText(..))

-- Since HarfBuzz language bindings neglected to declare these itself.
deriving instance Eq HB.Variation
deriving instance Ord HB.Variation
deriving instance Eq FontOptions
deriving instance Ord FontOptions

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
    atlases :: TMVar [(IS.IntSet, Atlas)]
    -- ^ Glyphs from the font rendered into GPU atleses.
  }

-- | Opens a font sized to given value & prepare to render text in it.
-- The fonts are cached for later reuse.
makeDrawTextCached :: (MonadIO m, MonadFail m, MonadError TypograffitiError m,
    MonadIO n, MonadFail n, MonadError TypograffitiError n) =>
    FontStore n -> FilePath -> Int -> GlyphSize -> SampleText ->
    m (RichText -> n (AllocatedRendering [TextTransform]))
makeDrawTextCached store filepath index fontsize SampleText {..} = do
    s <- liftIO $ atomically $ readTMVar $ fontMap store
    font <- case M.lookup (filepath, fontsize, index, fontOptions) s of
        Nothing -> allocFont store filepath index fontsize fontOptions
        Just font -> return font

    let glyphs = map (codepoint . fst) $
            shape (harfbuzz font) defaultBuffer {
                HB.text = Txt.replicate (toEnum $ succ $ length sampleFeatures) sampleText
            } sampleFeatures
    let glyphset = IS.fromList $ map fromEnum glyphs

    a <- liftIO $ atomically $ readTMVar $ atlases font
    atlas <- case [a' | (gs, a') <- a, glyphset `IS.isSubsetOf` gs] of
        (atlas:_) -> return atlas
        _ -> allocAtlas' (atlases font) (freetype font) glyphset

    return $ drawLinesWrapper tabwidth minLineHeight $
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

    atlases <- liftIO $ atomically $ newTMVar []
    let ret = Font font' font atlases

    atomically $ do
        map <- takeTMVar fontMap
        putTMVar fontMap $ M.insert (filepath, fontsize, index, options) ret map
    return ret
  where
    x2 = (*2)
    float2fixed :: Float -> FT_Fixed
    float2fixed = toEnum . fromEnum . (*65536)

-- | Allocates a new Atlas for the given font & glyphset,
-- loading it into the atlas cache before returning.
allocAtlas' :: (MonadIO m, MonadFail m, MonadError TypograffitiError m) =>
    TMVar [(IS.IntSet, Atlas)] -> FT_Face -> IS.IntSet -> m Atlas
allocAtlas' atlases font glyphset = do
    let glyphs = map toEnum $ IS.toList glyphset
    atlas <- allocAtlas (glyphRetriever font) glyphs

    liftIO $ atomically $ do
        a <- takeTMVar atlases
        putTMVar atlases $ ((glyphset, atlas):a)
    return atlas

-- | Runs the given callback with a new `FontStore`.
-- Due to FreeType limitations this font store should not persist outside the callback.
withFontStore :: (MonadIO n, MonadError TypograffitiError n, MonadFail n) =>
    (FontStore n -> ExceptT TypograffitiError IO a) ->
    IO (Either TypograffitiError a)
withFontStore cb = ft_With_FreeType $ \lib -> runExceptT $ (newFontStore lib >>= cb)

-- | Allocates a new FontStore wrapping given FreeType state.
newFontStore :: (MonadIO m, MonadError TypograffitiError m,
    MonadIO n, MonadError TypograffitiError n, MonadFail n) => FT_Library -> m (FontStore n)
newFontStore lib = do
    drawGlyphs <- makeDrawGlyphs
    store <- liftIO $ atomically $ newTMVar M.empty

    return $ FontStore store drawGlyphs lib
