-- |
-- Module:     Typograffiti.Atlas
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides a font-character atlas to use in font rendering with
-- opengl.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Typograffiti.GL.Atlas where

import           Control.Monad
import           Control.Monad.Except                              (MonadError (..))
import           Control.Monad.IO.Class
import           Data.IntMap                                       (IntMap)
import qualified Data.IntMap                                       as IM
import           Data.Vector.Unboxed                               (Vector)
import qualified Data.Vector.Unboxed                               as UV
import           Foreign.Marshal.Utils                             (with)
import           Graphics.GL.Core32
import           Graphics.GL.Types
import           Graphics.Rendering.FreeType.Internal.Bitmap       as BM
import           Graphics.Rendering.FreeType.Internal.GlyphMetrics as GM
import           Linear

import           Typograffiti.Atlas                                (Atlas (..), AtlasMeasure (..),
                                                                    emptyAM)
import           Typograffiti.Glyph                                (CharSize (..),
                                                                    GlyphMetrics (..),
                                                                    GlyphSize (..))

import           Typograffiti.Freetype
import           Typograffiti.GL.Utils.OpenGL


--------------------------------------------------------------------------------
-- Atlas
--------------------------------------------------------------------------------


type GLAtlas
  = Atlas GLuint (FT_Library, FT_Face)


texturize
  :: IntMap (V2 Int)
  -> Atlas GLuint (FT_Library, FT_Face)
  -> Char
  -> FreeTypeIO (Atlas GLuint (FT_Library, FT_Face))
texturize xymap atlas char
  | Just pos@(V2 x y) <- IM.lookup (fromEnum char) xymap = do
    (bmp, ftms) <- getFreetypeChar atlas char
    -- Update our texture by adding the bitmap
    glTexSubImage2D
      GL_TEXTURE_2D
      0
      (fromIntegral x)
      (fromIntegral y)
      (fromIntegral $ BM.width bmp)
      (fromIntegral $ rows bmp)
      GL_RED
      GL_UNSIGNED_BYTE
      (castPtr $ buffer bmp)
    -- Add the metrics to the atlas
    let vecwh = fromIntegral <$> V2 (BM.width bmp) (rows bmp)
        canon = floor @Double @Int . (* 0.015625) . fromIntegral
        vecsz = canon <$> V2 (GM.width ftms) (GM.height ftms)
        vecxb = canon <$> V2 (horiBearingX ftms) (horiBearingY ftms)
        vecyb = canon <$> V2 (vertBearingX ftms) (vertBearingY ftms)
        vecad = canon <$> V2 (horiAdvance ftms) (vertAdvance ftms)
        mtrcs = GlyphMetrics { glyphTexBB = (pos, pos + vecwh)
                             , glyphTexSize = vecwh
                             , glyphSize = vecsz
                             , glyphHoriBearing = vecxb
                             , glyphVertBearing = vecyb
                             , glyphAdvance = vecad
                             }
    return atlas{ atlasMetrics = IM.insert (fromEnum char) mtrcs (atlasMetrics atlas) }

  | otherwise = do
    liftIO $ putStrLn "could not find xy"
    return atlas


-- | Allocate a new 'Atlas'.
-- When creating a new 'Atlas' you must pass all the characters that you
-- might need during the life of the 'Atlas'. Character texturization only
-- happens once.
allocAtlas
  :: ( MonadIO m
     , MonadError String m
     )
  => FilePath
  -- ^ Path to the font file to use for this Atlas.
  -> GlyphSize
  -- ^ Size of glyphs in this Atlas.
  -> String
  -- ^ The characters to include in this 'Atlas'.
  -> m (Atlas GLuint (FT_Library, FT_Face))
allocAtlas fontFilePath gs str = do
  e <- liftIO $ runFreeType $ do
    fce <- newFace fontFilePath
    case gs of
      GlyphSizeInPixels w h -> setPixelSizes fce w h
      GlyphSizeByChar (CharSize w h dpix dpiy) -> setCharSize fce w h dpix dpiy

    (amMap, am) <- foldM (measure fce 512) (mempty, emptyAM) str

    let V2 w h = amWH am
        xymap :: IntMap (V2 Int)
        xymap  = amXY <$> amMap

    t <- liftIO $ do
      t <- allocAndActivateTex GL_TEXTURE0
      glPixelStorei GL_UNPACK_ALIGNMENT 1
      withCString (replicate (w * h) $ toEnum 0) $
        glTexImage2D GL_TEXTURE_2D 0 GL_RED (fromIntegral w) (fromIntegral h)
                     0 GL_RED GL_UNSIGNED_BYTE . castPtr
      return t

    lib   <- getLibrary
    atlas <- foldM (texturize xymap) (emptyAtlas lib fce t) str

    glGenerateMipmap GL_TEXTURE_2D
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glBindTexture GL_TEXTURE_2D 0
    glPixelStorei GL_UNPACK_ALIGNMENT 4
    return
      atlas{ atlasTextureSize = V2 w h
           , atlasGlyphSize = gs
           , atlasFilePath = fontFilePath
           }

  either
    (throwError . ("Cannot alloc atlas: " ++) . show)
    (return . fst)
    e


-- | Releases all resources associated with the given 'Atlas'.
freeAtlas :: MonadIO m => Atlas GLuint (FT_Library, FT_Face) -> m ()
freeAtlas a = liftIO $ do
  _ <- ft_Done_FreeType (atlasLibrary a)
  -- _ <- unloadMissingWords a ""
  with (atlasTexture a) $ \ptr -> glDeleteTextures 1 ptr
