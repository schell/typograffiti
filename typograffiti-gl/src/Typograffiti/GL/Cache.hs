{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Provides a method of caching rendererd text, making it suitable
-- for interactive rendering.
module Typograffiti.GL.Cache where

import           Control.Monad.Except         (MonadError (..), liftEither,
                                               runExceptT)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B8
import qualified Data.Vector.Unboxed          as UV
import           Foreign.Marshal.Array
import           Graphics.GL
import           Linear

import qualified Typograffiti                 as Core
import           Typograffiti.Atlas           (Atlas (..))
import           Typograffiti.Cache           (AllocatedRendering (..),
                                               WordCache)
import           Typograffiti.Transform       (Affine (..), Transform (..))

import           Typograffiti.Freetype        (FT_Face, FT_Library, boundingBox, stringTris)
import           Typograffiti.GL.Transform    (Multiply (..), TextTransform,
                                               translate)
import           Typograffiti.GL.Utils.OpenGL (bufferGeometry,
                                               compileOGLProgram,
                                               compileOGLShader, drawVAO,
                                               getUniformLocation, mat4Rotate,
                                               mat4Scale, mat4Translate,
                                               newBoundVAO, newBuffer,
                                               orthoProjection, updateUniform,
                                               withBoundTextures)


-- | Load the given text into the given WordCache using the given monadic
-- rendering and transform operations.
-- This is a specialized version of Typograffiti.Cache.loadText.
loadText
  :: ( MonadIO m
     , MonadError String m
     )
  => ( Atlas GLuint (FT_Library, FT_Face)
       -> String
       -> m (AllocatedRendering [TextTransform] GLuint)
     )
  -- ^ Monadic operation used to allocate a word.
  -> Atlas GLuint (FT_Library, FT_Face)
  -- ^ The character atlas that holds our letters.
  -> WordCache Char [TextTransform] GLuint
  -- ^ The WordCache to load AllocatedRenderings into.
  -> String
  -- ^ The string to render.
  -- This string may contain newlines, which will be respected.
  -> m ([TextTransform] -> IO (), V2 Int, WordCache Char [TextTransform] GLuint)
  -- ^ Returns a function for rendering the text, the size of the text and the
  -- new WordCache with the allocated renderings of the text.
loadText = Core.loadWords translate Core.charGlyphAction


transformToUniforms
  :: [TextTransform]
  -> (M44 Float, V4 Float)
transformToUniforms = foldl toUniform (identity, 1.0)
  where toUniform (mv, clr) (Transform (Multiply c)) =
          (mv, clr * c)
        toUniform (mv, clr) (TransformAffine s) =
          let mv1 = case s of
                AffineTranslate (V2 x y) ->
                  mv !*! mat4Translate (V3 x y 0)
                AffineScale (V2 x y) ->
                  mv !*! mat4Scale (V3 x y 1)
                AffineRotate r ->
                  mv !*! mat4Rotate r (V3 0 0 1)
          in (mv1, clr)


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


liftGL
  :: ( MonadIO m
     , MonadError String m
     )
  => m (Either String a)
  -> m a
liftGL m = m >>= liftEither


-- | A default operation for allocating one word worth of geometry. This is "word" as in
-- an English word, not a data type.
makeDefaultAllocateWord
  :: ( MonadIO m
     , MonadError String m
     , Integral i
     )
  => IO (V2 i)
  -- ^ A monadic operation that returns the current context's dimentions.
  -- This is used to set the orthographic projection for rendering text.
  -> m (Atlas GLuint (FT_Library, FT_Face)
        -> String
        -> IO (Either String (AllocatedRendering [TextTransform] GLuint))
       )
makeDefaultAllocateWord getContextSize = do
  let position = 0
      uv       = 1
  vert <- liftGL $ compileOGLShader vertexShader GL_VERTEX_SHADER
  frag <- liftGL $ compileOGLShader fragmentShader GL_FRAGMENT_SHADER
  prog <- liftGL $ compileOGLProgram
    [ ("position", fromIntegral position)
    , ("uv", fromIntegral uv)
    ]
    [vert, frag]
  glUseProgram prog
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  -- Get our uniform locations
  pjU    <- getUniformLocation prog "projection"
  mvU    <- getUniformLocation prog "modelview"
  multU  <- getUniformLocation prog "mult_color"
  texU   <- getUniformLocation prog "tex"
  -- Return a function that will generate new words
  return $ \atlas string -> do
    vao   <- newBoundVAO
    pbuf  <- newBuffer
    uvbuf <- newBuffer
    -- Generate our string geometry
    runExceptT (stringTris atlas True string) >>= \case
      Left err -> return $ Left err
      Right geom -> do
        let (ps, uvs) = UV.unzip geom
        -- Buffer the geometry into our attributes
        bufferGeometry position pbuf  ps
        bufferGeometry uv       uvbuf uvs
        glBindVertexArray 0

        let draw :: [TextTransform] -> IO ()
            draw ts = do
              let (mv, multVal) = transformToUniforms ts
              glUseProgram prog
              wsz <- getContextSize
              let pj :: M44 Float = orthoProjection wsz
              updateUniform prog pjU pj
              updateUniform prog mvU  mv
              updateUniform prog multU multVal
              updateUniform prog texU (0 :: Int)
              glBindVertexArray vao
              withBoundTextures [atlasTexture atlas] $ do
                drawVAO
                  prog
                  vao
                  GL_TRIANGLES
                  (fromIntegral $ UV.length ps)
                glBindVertexArray 0

            release = do
              withArray [pbuf, uvbuf] $ glDeleteBuffers 2
              withArray [vao] $ glDeleteVertexArrays 1
            (tl, br) = boundingBox ps

            size = br - tl
        return
          $ Right AllocatedRendering
              { arDraw     = draw
              , arTextures = []
              , arRelease  = release
              , arSizes    = [round <$> size]
              }
