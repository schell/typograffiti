{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Graphics.Text.Font.Render where

import           Data.Map (Map)
import           Data.Int (Int32)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Linear.V2 (V2(..))
import           Linear.V (toV, dim, Finite, Size)
import           FreeType.Core.Base (FT_Library, FT_Face, FT_FaceRec(..), ft_Load_Glyph,
                                    FT_GlyphSlotRec(..), FT_Glyph_Metrics(..))
import qualified FreeType.Core.Base as FT
import           FreeType.Core.Types (FT_Bitmap(..))
import           Data.Text.Glyphize (GlyphInfo(..), GlyphPos(..),
                                    shape, Buffer(..), defaultBuffer, ftCreateFont)

import           Graphics.GL as GL
import qualified Graphics.GL.Core32 as GL
import           Control.Monad (foldM, when)
import           Control.Exception (assert, Exception)
import qualified Data.Foldable          as F
import           GHC.TypeNats (KnownNat)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Linear.Matrix (M44, (!*!), identity, mkTransformationMat, mkTransformation)
import Linear.Quaternion (axisAngle)
import Linear.Projection (ortho)
import Data.List (foldl')

import           Foreign.Ptr (castPtr, nullPtr)
import           Foreign.C.String (withCString, peekCAStringLen)
import           Foreign.Marshal.Array (peekArray, allocaArray, withArray)
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
    glyphSize :: V2 Int
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

                tl = (V2 (x) y, V2 (texL/aszW) (texT/aszH))
                tr = (V2 (x+w) y, V2 (texR/aszW) (texT/aszH))
                br = (V2 (x+w) (y+h), V2 (texR/aszW) (texB/aszH))
                bl = (V2 (x) (y+h), V2 (texL/aszW) (texB/aszH))

            return (penx + f x_advance, peny + f y_advance,
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

makeDrawGlyphs getContextSize = do
    let position = 0
        uv = 1
    vert <- liftGL $ compileOGLShader vertexShader GL_VERTEX_SHADER
    frag <- liftGL $ compileOGLShader fragmentShader GL_FRAGMENT_SHADER
    prog <- liftGL $ compileOGLProgram [
        ("position", fromIntegral position),
        ("uv", fromIntegral uv)
      ] [vert, frag]
    glUseProgram prog
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    -- Get uniform locations
    pjU   <- getUniformLocation prog "projection"
    mvU   <- getUniformLocation prog "modelview"
    multU <- getUniformLocation prog "mult_color"
    texU  <- getUniformLocation prog "tex"
    return $ \atlas glyphs -> do
        vao   <- newBoundVAO
        pbuf  <- newBuffer
        uvbuf <- newBuffer
        (ps, uvs) <- unzip <$> stringTris' atlas glyphs
        bufferGeometry position pbuf $ UV.fromList ps
        bufferGeometry uv uvbuf $ UV.fromList uvs
        glBindVertexArray 0

        let draw ts = do
                let (mv, multVal) = transformToUniforms ts
                glUseProgram prog
                wsz <- getContextSize
                let pj = orthoProjection wsz
                updateUniform prog pjU pj
                updateUniform prog mvU mv
                updateUniform prog multU multVal
                updateUniform prog texU (0 :: Int)
                glBindVertexArray vao
                withBoundTextures [atlasTexture atlas] $ do
                    drawVAO prog vao GL_TRIANGLES (fromIntegral $ length ps)
                    glBindVertexArray 0
            release = do
                withArray [pbuf, uvbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
            (tl, br) = boundingBox ps
            size = br - tl
        return ()

vertexShader :: ByteString
vertexShader = B8.pack $ unlines
  [ "#version 330 core"
  , "uniform mat4 projection;"
  , "uniform mat4 modelview;"
  , "in vec2 position;"
  , "in vec2 uv;"
  , "out vec2 fuv;"
  , "void main () {"
  , "  fuv = uv;"
  , "  gl_Position = projection * modelview * vec4(position.xy, 0.0, 1.0);"
  , "}"
  ]

fragmentShader :: ByteString
fragmentShader = B8.pack $ unlines
  [ "#version 330 core"
  , "in vec2 fuv;"
  , "out vec4 fcolor;"
  , "uniform sampler2D tex;"
  , "uniform vec4 mult_color;"
  , "void main () {"
  , "  vec4 tcolor = texture(tex, fuv);"
  , "  fcolor = vec4(mult_color.rgb, mult_color.a * tcolor.r);"
  , "}"
  ]


------
--- Transforms
------

data SpatialTransform = SpatialTransformTranslate (V2 Float)
                      | SpatialTransformScale (V2 Float)
                      | SpatialTransformRotate Float


data TextTransform = TextTransformMultiply (V4 Float)
                   | TextTransformSpatial SpatialTransform


transformToUniforms :: [TextTransform] -> (M44 Float, V4 Float)
transformToUniforms = foldl toUniform (identity, 1.0)
  where toUniform (mv, clr) (TextTransformMultiply c) =
          (mv, clr * c)
        toUniform (mv, clr) (TextTransformSpatial s) =
          let mv1 = case s of
                SpatialTransformTranslate (V2 x y) ->
                  mv !*! mat4Translate (V3 x y 0)
                SpatialTransformScale (V2 x y) ->
                  mv !*! mat4Scale (V3 x y 1)
                SpatialTransformRotate r ->
                  mv !*! mat4Rotate r (V3 0 0 1)
          in (mv1, clr)

mat4Translate :: Num a => V3 a -> M44 a
mat4Translate = mkTransformationMat identity

mat4Rotate phi v = mkTransformation (axisAngle v phi) (V3 0 0 0)


mat4Scale :: Num a => V3 a -> M44 a
mat4Scale (V3 x y z) =
    V4 (V4 x 0 0 0)
       (V4 0 y 0 0)
       (V4 0 0 z 0)
       (V4 0 0 0 1)

orthoProjection :: Integral a => V2 a -> M44 Float
orthoProjection (V2 ww wh) =
  let (hw,hh) = (fromIntegral ww, fromIntegral wh)
  in ortho 0 hw hh 0 0 1


------
--- OpenGL Utilities
------

newBuffer :: IO GLuint
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

compileOGLShader
  :: ByteString
     -- ^ The shader source
  -> GLenum
  -- ^ The shader type (vertex, frag, etc)
  -> IO (Either String GLuint)
  -- ^ Either an error message or the generated shader handle.
compileOGLShader src shType = do
  shader <- glCreateShader shType
  if shader == 0
    then return $ Left "Could not create shader"
    else do
      success <- do
        withCString (B8.unpack src) $ \ptr ->
          with ptr $ \ptrptr -> glShaderSource shader 1 ptrptr nullPtr

        glCompileShader shader
        with (0 :: GLint) $ \ptr -> do
          glGetShaderiv shader GL_COMPILE_STATUS ptr
          peek ptr

      if success == GL_FALSE
        then do
          err <- do
            infoLog <- with (0 :: GLint) $ \ptr -> do
                glGetShaderiv shader GL_INFO_LOG_LENGTH ptr
                logsize <- peek ptr
                allocaArray (fromIntegral logsize) $ \logptr -> do
                    glGetShaderInfoLog shader logsize nullPtr logptr
                    peekArray (fromIntegral logsize) logptr

            return $ unlines [ "Could not compile shader:"
                             , B8.unpack src
                             , map (toEnum . fromEnum) infoLog
                             ]
          return $ Left err
        else return $ Right shader

compileOGLProgram
  :: [(String, Integer)]
  -> [GLuint]
  -> IO (Either String GLuint)
compileOGLProgram attribs shaders = do
  (program, success) <- do
     program <- glCreateProgram
     F.forM_ shaders (glAttachShader program)
     F.forM_ attribs
       $ \(name, loc) ->
         withCString name
           $ glBindAttribLocation program
           $ fromIntegral loc
     glLinkProgram program

     success <- with (0 :: GLint) $ \ptr -> do
       glGetProgramiv program GL_LINK_STATUS ptr
       peek ptr
     return (program, success)

  if success == GL_FALSE
  then with (0 :: GLint) $ \ptr -> do
    glGetProgramiv program GL_INFO_LOG_LENGTH ptr
    logsize <- peek ptr
    infoLog <- allocaArray (fromIntegral logsize) $ \logptr -> do
      glGetProgramInfoLog program logsize nullPtr logptr
      peekArray (fromIntegral logsize) logptr
    return
      $ Left
      $ unlines
          [ "Could not link program"
          , map (toEnum . fromEnum) infoLog
          ]
  else do
    F.forM_ shaders glDeleteShader
    return $ Right program

newBoundVAO :: IO GLuint
newBoundVAO = do
  [vao] <- allocaArray 1 $ \ptr -> do
      glGenVertexArrays 1 ptr
      peekArray 1 ptr
  glBindVertexArray vao
  return vao

drawVAO
  :: GLuint
  -- ^ The program
  -> GLuint
  -- ^ The vao
  -> GLenum
  -- ^ The draw mode
  -> GLsizei
  -- ^ The number of vertices to draw
  -> IO ()
drawVAO program vao mode num = do
  glUseProgram program
  glBindVertexArray vao
  clearErrors "drawBuffer:glBindVertex"
  glDrawArrays mode 0 num
  clearErrors "drawBuffer:glDrawArrays"

withBoundTextures :: [GLuint] -> IO a -> IO a
withBoundTextures ts f = do
  mapM_ (uncurry bindTex) (zip ts [GL_TEXTURE0 ..])
  a <- f
  glBindTexture GL_TEXTURE_2D 0
  return a
  where bindTex tex u = glActiveTexture u >> glBindTexture GL_TEXTURE_2D tex


---

getUniformLocation :: GLuint -> String -> IO GLint
getUniformLocation program ident = withCString ident $ glGetUniformLocation program

class UniformValue a where
  updateUniform
    :: GLuint
    -- ^ The program
    -> GLint
    -- ^ The uniform location
    -> a
    -- ^ The value.
    -> IO ()

clearUniformUpdateError :: Show a => GLuint -> GLint -> a -> IO ()
clearUniformUpdateError prog loc val = glGetError >>= \case
  0 -> return ()
  e -> do
    let buf = replicate 256 ' '
    ident <- withCString buf
      $ \strptr -> with 0
      $ \szptr  -> do
        glGetActiveUniformName prog (fromIntegral loc) 256 szptr strptr
        sz <- peek szptr
        peekCAStringLen (strptr, fromIntegral sz)
    putStrLn $ unwords
      [ "Could not update uniform"
      , ident
      , "with value"
      , show val
      , ", encountered error (" ++ show e ++ ")"
      , show (GL_INVALID_OPERATION :: Integer, "invalid operation" :: String)
      , show (GL_INVALID_VALUE :: Integer, "invalid value" :: String)
      ]
    assert False $ return ()


instance UniformValue Bool where
  updateUniform p loc bool = do
    glUniform1i loc $ if bool then 1 else 0
    clearUniformUpdateError p loc bool

instance UniformValue Int where
  updateUniform p loc enum = do
    glUniform1i loc $ fromIntegral $ fromEnum enum
    clearUniformUpdateError p loc enum

instance UniformValue Float where
  updateUniform p loc float = do
    glUniform1f loc $ realToFrac float
    clearUniformUpdateError p loc float

instance UniformValue Double where
  updateUniform p loc d = do
    glUniform1f loc $ realToFrac d
    clearUniformUpdateError p loc d

instance UniformValue (V2 Float) where
  updateUniform p loc v = do
    let V2 x y = fmap realToFrac v
    glUniform2f loc x y
    clearUniformUpdateError p loc v

instance UniformValue (V3 Float) where
  updateUniform p loc v = do
    let V3 x y z = fmap realToFrac v
    glUniform3f loc x y z
    clearUniformUpdateError p loc v

instance UniformValue (V4 Float) where
  updateUniform p loc v = do
    let (V4 r g b a) = realToFrac <$> v
    glUniform4f loc r g b a
    clearUniformUpdateError p loc v

instance UniformValue (M44 Float) where
  updateUniform p loc val = do
    with val $ glUniformMatrix4fv loc 1 GL_TRUE . castPtr
    clearUniformUpdateError p loc val

instance UniformValue (V2 Int) where
  updateUniform p loc v =  do
    let V2 x y = fmap fromIntegral v
    glUniform2i loc x y
    clearUniformUpdateError p loc v

instance UniformValue (Int,Int) where
  updateUniform p loc = updateUniform p loc . uncurry V2

liftGL
  :: IO (Either String a)
  -> IO a
liftGL n = do
  let lft (Left msg) = error msg
      lft (Right a) = return a
  n >>= lft

boundingBox [] = (0,0)
boundingBox vs = foldl' f (br,tl) vs
  where mn a = min a . realToFrac
        mx a = max a . realToFrac
        f (a, b) c = (mn <$> a <*> c, mx <$> b <*> c)
        inf = 1/0
        ninf = (-1)/0
        tl = V2 ninf ninf
        br = V2 inf inf

------
--- Simple API (Abstracting Harfbuzz)
------

makeDrawText font features sampletext getContextSize = do
    font' <- ftCreateFont font
    let glyphs = map (codepoint . fst) $
            shape font' defaultBuffer { text = sampletext } features
    let glyphs' = map toEnum $ IS.toList $ IS.fromList $ map fromEnum glyphs
    atlas <- allocAtlas (glyphRetriever font) glyphs'

    drawGlyphs <- makeDrawGlyphs getContextSize
    return $ \string ->
        drawGlyphs atlas $ shape font' defaultBuffer { text = string } features
