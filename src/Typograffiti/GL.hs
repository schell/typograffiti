{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
module Typograffiti.GL where

import           Control.Exception      (assert)
import           Control.Monad          (forM_, when)
import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B8
import qualified Data.Foldable          as F
import qualified Data.Vector.Storable   as SV
import           Data.Vector.Unboxed    (Unbox)
import qualified Data.Vector.Unboxed    as UV
import           Foreign.C.String       (peekCAStringLen, withCString)
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.TypeLits           (KnownNat)
import           Graphics.GL.Core32
import           Graphics.GL.Types
import           Linear
import           Linear.V               (Finite, Size, dim, toV)



allocAndActivateTex :: GLenum -> IO GLuint
allocAndActivateTex u = do
    [t] <- allocaArray 1 $ \ptr -> do
        glGenTextures 1 ptr
        peekArray 1 ptr
    glActiveTexture u
    glBindTexture GL_TEXTURE_2D t
    return t


clearErrors :: String -> IO ()
clearErrors str = do
    err' <- glGetError
    when (err' /= 0) $ do
      putStrLn $ unwords [str, show err']
      assert False $ return ()


withVAO :: MonadIO m => (GLuint -> IO b) -> m b
withVAO f = liftIO $ do
  [vao] <- allocaArray 1 $ \ptr -> do
      glGenVertexArrays 1 ptr
      peekArray 1 ptr
  glBindVertexArray vao
  r <- f vao
  clearErrors "withVAO"
  glBindVertexArray 0
  return r


withBuffers :: Int -> ([GLuint] -> IO b) -> IO b
withBuffers n f = do
  bufs <- allocaArray n $ \ptr -> do
      glGenBuffers (fromIntegral n) ptr
      peekArray (fromIntegral n) ptr
  f bufs


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
    glBindBuffer GL_ARRAY_BUFFER buf
    SV.unsafeWith (convertVec as) $ \ptr ->
      glBufferData GL_ARRAY_BUFFER (fromIntegral asize) (castPtr ptr) GL_STATIC_DRAW
    glEnableVertexAttribArray loc
    glVertexAttribPointer loc n GL_FLOAT GL_FALSE 0 nullPtr
    clearErrors "bufferGeometry"


convertVec
  :: (Unbox (f Float), Foldable f) => UV.Vector (f Float) -> SV.Vector GLfloat
convertVec =
  SV.convert . UV.map realToFrac . UV.concatMap (UV.fromList . F.toList)


-- | Binds the given textures to GL_TEXTURE0, GL_TEXTURE1, ... in ascending
-- order of the texture unit, runs the IO action and then unbinds the textures.
withBoundTextures :: MonadIO m => [GLuint] -> m a -> m a
withBoundTextures ts f = do
  liftIO $ mapM_ (uncurry bindTex) (zip ts [GL_TEXTURE0 ..])
  a <- f
  liftIO $ glBindTexture GL_TEXTURE_2D 0
  return a
  where bindTex tex u = glActiveTexture u >> glBindTexture GL_TEXTURE_2D tex


drawVAO
  :: MonadIO m
  => GLuint
  -- ^ The program
  -> GLuint
  -- ^ The vao
  -> GLenum
  -- ^ The draw mode
  -> GLsizei
  -- ^ The number of vertices to draw
  -> m ()
drawVAO program vao mode num = liftIO $ do
  glUseProgram program
  glBindVertexArray vao
  clearErrors "drawBuffer:glBindVertex"
  glDrawArrays mode 0 num
  clearErrors "drawBuffer:glDrawArrays"


compileOGLShader
  :: (MonadIO m, MonadError String m)
  => ByteString
     -- ^ The shader source
  -> GLenum
  -- ^ The shader type (vertex, frag, etc)
  -> m GLuint
  -- ^ Either an error message or the generated shader handle.
compileOGLShader src shType = do
  shader <- liftIO $ glCreateShader shType
  if shader == 0
    then throwError "Could not create shader"
    else do
      success <- liftIO $ do
        withCString (B8.unpack src) $ \ptr ->
          with ptr $ \ptrptr -> glShaderSource shader 1 ptrptr nullPtr

        glCompileShader shader
        with (0 :: GLint) $ \ptr -> do
          glGetShaderiv shader GL_COMPILE_STATUS ptr
          peek ptr

      if success == GL_FALSE
        then do
          err <- liftIO $ do
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
          throwError err
        else return shader


compileOGLProgram
  :: ( MonadIO m
     , MonadError String m
     )
  => [(String, Integer)]
  -> [GLuint]
  -> m GLuint
compileOGLProgram attribs shaders = do
  (program, success) <- liftIO $ do
     program <- glCreateProgram
     forM_ shaders (glAttachShader program)
     forM_ attribs
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
  then do
    err <- liftIO $ with (0 :: GLint) $ \ptr -> do
      glGetProgramiv program GL_INFO_LOG_LENGTH ptr
      logsize <- peek ptr
      infoLog <- allocaArray (fromIntegral logsize) $ \logptr -> do
        glGetProgramInfoLog program logsize nullPtr logptr
        peekArray (fromIntegral logsize) logptr
      return $ unlines [ "Could not link program"
                        , map (toEnum . fromEnum) infoLog
                        ]
    throwError err
  else do
    liftIO $ forM_ shaders glDeleteShader
    return program


--------------------------------------------------------------------------------
-- Uniform marshaling functions
--------------------------------------------------------------------------------


getUniformLocation :: MonadIO m => GLuint -> String -> m GLint
getUniformLocation program ident = liftIO
  $ withCString ident
  $ glGetUniformLocation program


class UniformValue a where
  updateUniform
    :: MonadIO m
    => GLuint
    -- ^ The program
    -> GLint
    -- ^ The uniform location
    -> a
    -- ^ The value.
    -> m ()


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
    putStrLn $ unwords [ "Could not update uniform"
                        , ident
                        , "with value"
                        , show val
                        , ", encountered error (" ++ show e ++ ")"
                        , show (GL_INVALID_OPERATION :: Integer, "invalid operation" :: String)
                        , show (GL_INVALID_VALUE :: Integer, "invalid value" :: String)
                        ]
    assert False $ return ()


instance UniformValue Bool where
  updateUniform p loc bool = liftIO $ do
    glUniform1i loc $ if bool then 1 else 0
    clearUniformUpdateError p loc bool

instance UniformValue Int where
  updateUniform p loc enum = liftIO $ do
    glUniform1i loc $ fromIntegral $ fromEnum enum
    clearUniformUpdateError p loc enum

instance UniformValue Float where
  updateUniform p loc float = liftIO $ do
    glUniform1f loc $ realToFrac float
    clearUniformUpdateError p loc float

instance UniformValue Double where
  updateUniform p loc d = liftIO $ do
    glUniform1f loc $ realToFrac d
    clearUniformUpdateError p loc d

instance UniformValue (V2 Float) where
  updateUniform p loc v = liftIO $ do
    let V2 x y = fmap realToFrac v
    glUniform2f loc x y
    clearUniformUpdateError p loc v

instance UniformValue (V3 Float) where
  updateUniform p loc v = liftIO $ do
    let V3 x y z = fmap realToFrac v
    glUniform3f loc x y z
    clearUniformUpdateError p loc v

instance UniformValue (V4 Float) where
  updateUniform p loc v = liftIO $ do
    let (V4 r g b a) = realToFrac <$> v
    glUniform4f loc r g b a
    clearUniformUpdateError p loc v

instance UniformValue (M44 Float) where
  updateUniform p loc val = liftIO $ do
    with val $ glUniformMatrix4fv loc 1 GL_TRUE . castPtr
    clearUniformUpdateError p loc val

instance UniformValue (V2 Int) where
  updateUniform p loc v = liftIO $ do
    let V2 x y = fmap fromIntegral v
    glUniform2i loc x y
    clearUniformUpdateError p loc v

instance UniformValue (Int,Int) where
  updateUniform p loc = updateUniform p loc . uncurry V2


--------------------------------------------------------------------------------
-- Matrix helpers
--------------------------------------------------------------------------------


mat4Translate :: Num a => V3 a -> M44 a
mat4Translate = mkTransformationMat identity


mat4Rotate :: (Num a, Epsilon a, Floating a) => a -> V3 a -> M44 a
mat4Rotate phi v = mkTransformation (axisAngle v phi) (V3 0 0 0)


mat4Scale :: Num a => V3 a -> M44 a
mat4Scale (V3 x y z) =
    V4 (V4 x 0 0 0)
       (V4 0 y 0 0)
       (V4 0 0 z 0)
       (V4 0 0 0 1)


orthoProjection
  :: Integral a
  => V2 a
  -- ^ The window width and height
  -> M44 Float
orthoProjection (V2 ww wh) =
  let (hw,hh) = (fromIntegral ww, fromIntegral wh)
  in ortho 0 hw hh 0 0 1
