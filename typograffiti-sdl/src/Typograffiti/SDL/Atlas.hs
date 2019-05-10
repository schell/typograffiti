-- |
-- This module provides a font-character atlas to use in font rendering with
-- sdl2's built in 2d renderer.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Typograffiti.SDL.Atlas where

import           Control.Monad
import           Control.Monad.Except                              (MonadError (..))
import           Control.Monad.IO.Class
import qualified Data.ByteString                                   as BS
import           Data.IntMap                                       (IntMap)
import qualified Data.IntMap                                       as IM
import           Data.Vector.Unboxed                               (Vector)
import qualified Data.Vector.Unboxed                               as UV
import           Foreign.Marshal.Utils                             (with)
import           Graphics.Rendering.FreeType.Internal.Bitmap       as BM
import           Graphics.Rendering.FreeType.Internal.GlyphMetrics as GM
import           Linear
import           SDL                                               (Renderer,
                                                                    Texture,
                                                                    ($=))
import qualified SDL

import           Typograffiti.Atlas                                (Atlas (..), AtlasMeasure (..),
                                                                    emptyAM)
import           Typograffiti.Freetype
import           Typograffiti.Glyph                                (CharSize (..),
                                                                    GlyphMetrics (..),
                                                                    GlyphSize (..))


--------------------------------------------------------------------------------
-- Atlas
--------------------------------------------------------------------------------


type SDLAtlas
  = Atlas Texture (FT_Library, FT_Face)


texturize
  :: Texture
  -> IntMap (V2 Int)
  -> Atlas Texture (FT_Library, FT_Face)
  -> Char
  -> FreeTypeIO (Atlas Texture (FT_Library, FT_Face))
texturize tex xymap atlas char
  | Just pos@(V2 x y) <- IM.lookup (fromEnum char) xymap = do
    (bmp, ftms) <- getFreetypeChar atlas char
    -- Update our texture by adding the bitmap
    let dest = SDL.Rectangle (SDL.P pos) (fromIntegral <$> V2 (BM.width bmp) (rows bmp))
    bytes <-
      liftIO
        $ BS.packCStringLen (BM.buffer bmp, fromIntegral $ BM.width bmp * rows bmp)
    let rgbaBytes =
          flip BS.concatMap
            bytes
            $ \val -> foldr BS.cons BS.empty [val,255,255,255]
    liftIO $ print (char, V2 (BM.width bmp) (rows bmp), BM.pitch bmp)
    unless (BM.pitch bmp == 0)
      $ void
      $ SDL.updateTexture
          tex
          (Just $ fromIntegral <$> dest)
          rgbaBytes
          (BM.width bmp * 4)
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
-- might need during the life of the 'Atlas'. Glyph texturization only
-- happens once.
allocAtlas
  :: ( MonadIO m
     , MonadError String m
     )
  => Renderer
  -- ^ The SDL 2d renderer.
  -> FilePath
  -- ^ Path to the font file to use for this Atlas.
  -> GlyphSize
  -- ^ Size of glyphs in this Atlas.
  -> String
  -- ^ The characters to include in this 'Atlas'.
  -> m (Atlas Texture (FT_Library, FT_Face))
allocAtlas r fontFilePath gs str = do
  e <- liftIO $ runFreeType $ do
    fce <- newFace fontFilePath
    case gs of
      GlyphSizeInPixels w h -> setPixelSizes fce w h
      GlyphSizeByChar (CharSize w h dpix dpiy) -> setCharSize fce w h dpix dpiy

    (amMap, am) <- foldM (measure fce 512) (mempty, emptyAM) str

    let V2 w h = amWH am
        xymap :: IntMap (V2 Int)
        xymap  = amXY <$> amMap
    liftIO $ print ("allocAtlas", str, V2 w h)
    t <-
      SDL.createTexture
        r
        SDL.RGBA8888
        SDL.TextureAccessStreaming
        (fromIntegral <$> V2 w h)
    --SDL.textureBlendMode t $= SDL.BlendAlphaBlend
    lib   <- getLibrary
    atlas <- foldM (texturize t xymap) (emptyAtlas lib fce t) str
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
freeAtlas :: MonadIO m => Atlas Texture (FT_Library, FT_Face) -> m ()
freeAtlas a = liftIO $ do
  _ <- ft_Done_FreeType (atlasLibrary a)
  -- _ <- unloadMissingWords a ""
  SDL.destroyTexture (atlasTexture a)
