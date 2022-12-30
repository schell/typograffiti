{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts  #-}
module Graphics.Text.Font.Render where

import           Data.Map (Map)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Linear.V2 (V2(..))
import           Linear.V (toV, dim, Finite, Size)
import           FreeType.Core.Base (FT_Library, FT_Face,
                                    FT_GlyphSlotRec(..), FT_Glyph_Metrics(..))
import           FreeType.Core.Types (FT_Bitmap(..))

import           Graphics.GL as GL
import qualified Graphics.GL.Core32 as GL
import           Control.Monad (foldM, when)
import           Control.Exception (assert)
import qualified Data.Foldable          as F
import           GHC.TypeNats (KnownNat)

import           Foreign.Ptr (castPtr, nullPtr)
import           Foreign.C.String (withCString)
import           Foreign.Marshal.Array (peekArray, allocaArray)
import           Foreign.Marshal.Utils (with)
import           Foreign.Storable (Storable(..))
import qualified Data.Vector.Storable   as SV
import           Data.Vector.Unboxed    (Unbox)
import qualified Data.Vector.Unboxed    as UV

------
--- Atlas
------

data GlyphMetrics = GlyphMetrics {
    glyphTexBB :: (V2 Int, V2 Int),
    glyphTexSize :: V2 Int,
    glyphSize :: V2 Int,
    glyphHoriBearing :: V2 Int,
    glyphVertBearing :: V2 Int,
    glyphAdvance :: V2 Int
} deriving (Show, Eq)

data Atlas = Atlas {
    atlasTexture :: GLuint,
    atlasTextureSize :: V2 Int,
    atlasMetrics :: IntMap GlyphMetrics,
    atlasFilePath :: FilePath
}

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
                glyphSize = vecsz,
                glyphHoriBearing = vecxb,
                glyphVertBearing = vecyb,
                glyphAdvance = vecad
              }
        return atlas { atlasMetrics = IM.insert (fromEnum glyph) mtrcs atlasMetrics }
    | otherwise = do
        putStrLn ("Cound not find glyph " ++ show glyph)
        return atlas

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

makeCharQuad Atlas {..} (penx, mLast) glyph = do
    let iglyph = fromEnum glyph
    case IM.lookup iglyph atlasMetrics of
        Nothing -> return (penx, mLast)
        Just GlyphMetrics {..} -> do
            -- TODO incorporate Harfbuzz positioning.
            let V2 dx dy = fromIntegral <$> glyphHoriBearing
                x = (fromIntegral penx) + dx
                y = -dy
                V2 w h = fromIntegral <$> glyphSize
                V2 aszW aszH = fromIntegral <$> atlasTextureSize
                V2 texL texT = fromIntegral <$> fst glyphTexBB
                V2 texR texB = fromIntegral <$> snd glyphTexBB

                tl = (V2 (x) y, V2 (texL/aszW) (texT/aszH))
                tr = (V2 (x+w) y, V2 (texR/aszW) (texT/aszH))
                br = (V2 (x+w) (y+h), V2 (texR/aszW) (texB/aszH))
                bl = (V2 (x) (y+h), V2 (texL/aszW) (texB/aszH))
            let V2 ax _ = glyphAdvance

            return (penx + ax, mLast ++ [tl, tr, br, tl, br, bl])

stringTris atlas = foldM (makeCharQuad atlas) (0, [])

drawGlyphs atlas@Atlas {..} glyphs = do
    glBindTexture GL.GL_TEXTURE_2D atlasTexture

    (geom', texcoords') <- unzip <$> snd <$> stringTris atlas glyphs
    geom <- newBuffer
    texcoords <- newBuffer
    bufferGeometry 0 geom $ UV.fromList geom'
    bufferGeometry 1 texcoords $ UV.fromList texcoords'
    glDrawArrays GL.GL_TRIANGLES 0 $ toEnum $ SV.length $ convertVec $ UV.fromList geom'

------
--- OpenGL Utilities
------

newBuffer = do
  [b] <- allocaArray 1 $ \ptr -> do
    glGenBuffers 1 ptr
    peekArray 1 ptr
  return b

-- | Buffer some geometry into an attribute.
-- The type variable 'f' should be V0, V1, V2, V3 or V4.
bufferGeometry
  :: ( Foldable f
     , Unbox (f Float)
     , Storable (f Float)
     , Finite f
     , KnownNat (Size f)
     )
  => GLuint
  -- ^ The attribute location.
  -> GLuint
  -- ^ The buffer identifier.
  -> UV.Vector (f Float)
  -- ^ The geometry to buffer.
  -> IO ()
bufferGeometry loc buf as
  | UV.null as = return ()
  | otherwise = do
    let v     = UV.head as
        asize = UV.length as * sizeOf v
        n     = fromIntegral $ dim $ toV v
    glBindBuffer GL.GL_ARRAY_BUFFER buf
    SV.unsafeWith (convertVec as) $ \ptr ->
      glBufferData GL.GL_ARRAY_BUFFER (fromIntegral asize) (castPtr ptr) GL.GL_STATIC_DRAW
    glEnableVertexAttribArray loc
    glVertexAttribPointer loc n GL.GL_FLOAT GL.GL_FALSE 0 nullPtr
    clearErrors "bufferGeometry"

convertVec
  :: (Unbox (f Float), Foldable f) => UV.Vector (f Float) -> SV.Vector GLfloat
convertVec =
  SV.convert . UV.map realToFrac . UV.concatMap (UV.fromList . F.toList)

clearErrors str = do
  err' <- glGetError
  when (err' /= 0) $ do
    putStrLn $ unwords [str, show err']
    assert False $ return ()
