{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module:     Typograffiti.Atlas
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides a font-character atlas to use in font rendering with
-- opengl.
--
module Typograffiti.Atlas where

import           Control.Monad
import           Control.Monad.Except                              (MonadError (..))
import           Control.Monad.IO.Class
import           Data.Maybe                                        (fromMaybe)
import           Data.IntMap                                       (IntMap)
import qualified Data.IntMap                                       as IM
import           Data.Vector.Unboxed                               (Vector)
import qualified Data.Vector.Unboxed                               as UV
import           Foreign.Marshal.Utils                             (with)
import           Graphics.GL.Core32
import           Graphics.GL.Types
import           FreeType.Core.Types                               as BM
import           FreeType.Support.Bitmap                           as BM
import           FreeType.Support.Bitmap.Internal                  as BM
import           Linear

import           Typograffiti.GL
import           Typograffiti.Glyph
import           Typograffiti.Utils

------
--- Atlas
------

data GlyphMetrics = GlyphMetrics {
    glyphTexBB :: (V2 Int, V2 Int),
    glyphTexSize :: V2 Int,
    glyphSize :: V2 Int
} deriving (Show, Eq)

data Atlas = Atlas {
    atlasTexture :: GLuint,
    atlasTextureSize :: V2 Int,
    atlasMetrics :: IntMap GlyphMetrics,
    atlasFilePath :: FilePath
} deriving (Show)

emptyAtlas t = Atlas t 0 mempty ""

data AtlasMeasure = AM {
    amWH :: V2 Int,
    amXY :: V2 Int,
    rowHeight :: Int,
    amMap :: IntMap (V2 Int)
} deriving (Show, Eq)

emptyAM :: AtlasMeasure
emptyAM = AM 0 (V2 1 1) 0 mempty

spacing :: Int
spacing = 1

glyphRetriever font glyph = do
    ft_Load_Glyph font (fromIntegral $ fromEnum glyph) FT.FT_LOAD_RENDER
    font' <- peek font
    slot <- peek $ frGlyph font'
    return (gsrBitmap slot, gsrMetrics slot)

measure cb maxw am@AM{..} glyph
    | Just _ <- IM.lookup (fromEnum glyph) amMap = return am
    | otherwise = do
        let V2 x y = amXY
            V2 w h = amWH
        (bmp, _) <- cb glyph
        let bw = fromIntegral $ bWidth bmp
            bh = fromIntegral $ bRows bmp
            gotoNextRow = (x + bw + spacing >= maxw)
            rh = if gotoNextRow then 0 else max bh rowHeight
            nx = if gotoNextRow then 0 else x + bw + spacing
            nw = max w (x + bw + spacing)
            nh = max h (y + rh + spacing)
            ny = if gotoNextRow then nh else y
            am = AM {
                amWH = V2 nw nh,
                amXY = V2 nx ny,
                rowHeight = rh,
                amMap = IM.insert (fromEnum glyph) amXY amMap
              }
        return am

texturize cb xymap atlas@Atlas{..} glyph
    | Just pos@(V2 x y) <- IM.lookup (fromIntegral $ fromEnum glyph) xymap = do
        (bmp, metrics) <- cb glyph
        glTexSubImage2D GL.GL_TEXTURE_2D 0
            (fromIntegral x) (fromIntegral y)
            (fromIntegral $ bWidth bmp) (fromIntegral $ bRows bmp)
            GL.GL_RED GL.GL_UNSIGNED_BYTE
            (castPtr $ bBuffer bmp)
        let vecwh = fromIntegral <$> V2 (bWidth bmp) (bRows bmp)
            canon = floor . (* 0.5) . (* 0.015625) . realToFrac . fromIntegral
            vecsz = canon <$> V2 (gmWidth metrics) (gmHeight metrics)
            vecxb = canon <$> V2 (gmHoriBearingX metrics) (gmHoriBearingY metrics)
            vecyb = canon <$> V2 (gmVertBearingX metrics) (gmVertBearingY metrics)
            vecad = canon <$> V2 (gmHoriAdvance metrics) (gmVertAdvance metrics)
            mtrcs = GlyphMetrics {
                glyphTexBB = (pos, pos + vecwh),
                glyphTexSize = vecwh,
                glyphSize = vecsz
              }
        return atlas { atlasMetrics = IM.insert (fromEnum glyph) mtrcs atlasMetrics }
    | otherwise = do
        putStrLn ("Cound not find glyph " ++ show glyph)
        return atlas

allocAtlas :: (Int32 -> IO (FT_Bitmap, FT_Glyph_Metrics)) -> [Int32] -> IO Atlas
allocAtlas cb glyphs = do
    AM {..} <- foldM (measure cb 512) emptyAM glyphs
    let V2 w h = amWH
        xymap = amMap

    [t] <- allocaArray 1 $ \ptr -> do
        glGenTextures 1 ptr
        peekArray 1 ptr
    glActiveTexture 0
    glBindTexture GL.GL_TEXTURE_2D t

    glPixelStorei GL.GL_UNPACK_ALIGNMENT 1
    withCString (replicate (w * h) $ toEnum 0) $
        glTexImage2D GL.GL_TEXTURE_2D 0 GL.GL_RED (fromIntegral w) (fromIntegral h)
                    0 GL.GL_RED GL.GL_UNSIGNED_BYTE . castPtr
    atlas <- foldM (texturize cb xymap) (emptyAtlas t) glyphs

    glGenerateMipmap GL.GL_TEXTURE_2D
    glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S GL.GL_REPEAT
    glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T GL.GL_REPEAT
    glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER GL.GL_LINEAR
    glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER GL.GL_LINEAR
    glBindTexture GL.GL_TEXTURE_2D 0
    glPixelStorei GL.GL_UNPACK_ALIGNMENT 4
    return atlas { atlasTextureSize = V2 w h }

freeAtlas a = with (atlasTexture a) $ \ptr -> glDeleteTextures 1 ptr

type Quads = (Float, Float, [(V2 Float, V2 Float)])
makeCharQuad :: Atlas -> Quads -> (GlyphInfo, GlyphPos) -> IO Quads
makeCharQuad Atlas {..} (penx, peny, mLast) (GlyphInfo {codepoint=glyph}, GlyphPos {..}) = do
    let iglyph = fromEnum glyph
    case IM.lookup iglyph atlasMetrics of
        Nothing -> return (penx, peny, mLast)
        Just GlyphMetrics {..} -> do
            let x = penx + f x_offset
                y = peny + f y_offset
                V2 w h = f' <$> glyphSize
                V2 aszW aszH = f' <$> atlasTextureSize
                V2 texL texT = f' <$> fst glyphTexBB
                V2 texR texB = f' <$> snd glyphTexBB

                tl = (V2 (x) (y-h), V2 (texL/aszW) (texT/aszH))
                tr = (V2 (x+w) (y-h), V2 (texR/aszW) (texT/aszH))
                br = (V2 (x+w) y, V2 (texR/aszW) (texB/aszH))
                bl = (V2 (x) y, V2 (texL/aszW) (texB/aszH))

            return (penx + f x_advance/150, peny + f y_advance/150,
                    mLast ++ [tl, tr, br, tl, br, bl])
  where
    f :: Int32 -> Float
    f = fromIntegral
    f' :: Int -> Float
    f' = fromIntegral

stringTris :: Atlas -> [(GlyphInfo, GlyphPos)] -> IO Quads
stringTris atlas = foldM (makeCharQuad atlas) (0, 0, [])
stringTris' :: Atlas -> [(GlyphInfo, GlyphPos)] -> IO [(V2 Float, V2 Float)]
stringTris' atlas glyphs = do
    (_, _, ret) <- stringTris atlas glyphs
    return ret
