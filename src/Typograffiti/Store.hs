{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards  #-}
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
import           Control.Monad.Except   (MonadError (..), liftEither, runExceptT, ExceptT (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Fail     (MonadFail (..))
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Set               (Set)
import qualified Data.Set               as S
import qualified Data.IntSet            as IS
import           Linear
import qualified Data.ByteString        as B
import           Data.Text.Glyphize     (defaultBuffer, Buffer(..), shape,
                                        GlyphInfo(..), GlyphPos(..))
import qualified Data.Text.Glyphize     as HB
import           Data.Text.Lazy         (Text, pack)
import qualified Data.Text.Lazy         as Txt
import           FreeType.Core.Base

import           Typograffiti.Atlas
import           Typograffiti.Cache
import           Typograffiti.Text      (GlyphSize(..), drawLinesWrapper, SampleText(..))

data FontStore n = FontStore {
    fontMap :: TMVar (Map (FilePath, GlyphSize, Int) Font),
    drawGlyphs :: Atlas -> [(GlyphInfo, GlyphPos)] -> n (AllocatedRendering [TextTransform]),
    lib :: FT_Library
  }
data Font = Font {
    harfbuzz :: HB.Font,
    freetype :: FT_Face,
    atlases :: TMVar [(IS.IntSet, Atlas)]
  }

makeDrawTextCached :: (MonadIO m, MonadFail m, MonadError TypograffitiError m,
    MonadIO n, MonadFail n, MonadError TypograffitiError n) =>
    FontStore n -> FilePath -> Int -> GlyphSize -> SampleText ->
    m (String -> [HB.Feature] -> n (AllocatedRendering [TextTransform]))
makeDrawTextCached store filepath index fontsize SampleText {..} = do
    s <- liftIO $ atomically $ readTMVar $ fontMap store
    font <- case M.lookup (filepath, fontsize, index) s of
        Nothing -> allocFont store filepath index fontsize
        Just font -> return font

    let glyphs = map (codepoint . fst) $
            shape (harfbuzz font) defaultBuffer {
                text = Txt.replicate (toEnum $ succ $ length sampleFeatures) sampleText
            } sampleFeatures
    let glyphset = IS.fromList $ map fromEnum glyphs

    a <- liftIO $ atomically $ readTMVar $ atlases font
    atlas <- case [a' | (gs, a') <- a, glyphset `IS.isSubsetOf` gs] of
        (atlas:_) -> return atlas
        _ -> allocAtlas' (atlases font) (freetype font) glyphset

    return $ drawLinesWrapper tabwidth $ \string features -> drawGlyphs store atlas $
        shape (harfbuzz font) defaultBuffer { text = pack string } []

allocFont :: (MonadIO m) => FontStore n -> FilePath -> Int -> GlyphSize -> m Font
allocFont FontStore {..} filepath index fontsize = liftIO $ do
    font <- ft_New_Face lib filepath $ toEnum index
    case fontsize of
        PixelSize w h -> ft_Set_Pixel_Sizes font (toEnum $ x2 w) (toEnum $ x2 h)
        CharSize w h dpix dpiy -> ft_Set_Char_Size font (floor $ 26.6 * 2 * w)
                                                    (floor $ 26.6 * 2 * h)
                                                    (toEnum dpix) (toEnum dpiy)

    bytes <- B.readFile filepath
    let font' = HB.createFont $ HB.createFace bytes $ toEnum index

    atlases <- liftIO $ atomically $ newTMVar []
    let ret = Font font' font atlases

    atomically $ do
        map <- takeTMVar fontMap
        putTMVar fontMap $ M.insert (filepath, fontsize, index) ret map
    return ret
  where x2 = (*2)

allocAtlas' :: (MonadIO m, MonadFail m) =>
    TMVar [(IS.IntSet, Atlas)] -> FT_Face -> IS.IntSet -> m Atlas
allocAtlas' atlases font glyphset = do
    let glyphs = map toEnum $ IS.toList glyphset
    atlas <- allocAtlas (glyphRetriever font) glyphs

    liftIO $ atomically $ do
        a <- takeTMVar atlases
        putTMVar atlases $ ((glyphset, atlas):a)
    return atlas

withFontStore :: (MonadIO n, MonadError TypograffitiError n, MonadFail n) =>
    (FontStore n -> ExceptT TypograffitiError IO a) ->
    IO (Either TypograffitiError a)
withFontStore cb = ft_With_FreeType $ \lib -> runExceptT $ (newFontStore lib >>= cb)

newFontStore :: (MonadIO m, MonadError TypograffitiError m,
    MonadIO n, MonadError TypograffitiError n, MonadFail n) => FT_Library -> m (FontStore n)
newFontStore lib = do
    drawGlyphs <- makeDrawGlyphs
    store <- liftIO $ atomically $ newTMVar M.empty

    return $ FontStore store drawGlyphs lib
