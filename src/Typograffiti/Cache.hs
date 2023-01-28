{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
-- |
-- Module:     Typograffiti.Cache
-- Copyright:  (c) 2018 Schell Scivally, 2023 Adrian Cochrane
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--             & Adrian Cochrane <alcinnz@argonaut-constellation.org>
--
-- This module provides a method of caching rendererd text, making it suitable
-- for interactive rendering. You can use the defaultCache or provide your own.
--
module Typograffiti.Cache where

import           Control.Monad.Except   (MonadError (..), liftEither)
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Bifunctor         (first)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B8
import qualified Data.Vector.Unboxed    as UV
import           Foreign.Marshal.Array  (withArray)
import           Graphics.GL
import           Linear                 (V2 (..), V3 (..), V4 (..), M44 (..),
                                        (!*!), identity)
import           Data.Text.Glyphize     (GlyphInfo(..), GlyphPos(..))

import           Typograffiti.Atlas
import           Typograffiti.GL

-- | Generic operations for text layout.
class Layout t where
  translate :: t -> V2 Float -> t

-- | Holds an allocated draw function for some amount of text. The function
-- takes one parameter that can be used to transform the text in various ways.
-- This type is generic and can be used to take advantage of your own font
-- rendering shaders.
data AllocatedRendering t = AllocatedRendering
  { arDraw    :: t -> V2 Int -> IO ()
    -- ^ Draw the text with some transformation in some monad.
  , arRelease :: IO ()
    -- ^ Release the allocated draw function in some monad.
  , arSize    :: V2 Int
    -- ^ The size (in pixels) of the drawn text.
  }

-- | Constructs a callback for for computing the geometry for
-- rendering given glyphs out of the given texture.
makeDrawGlyphs
  :: ( MonadIO m
     , MonadError TypograffitiError m
     , MonadIO n
     , MonadFail n
     , MonadError TypograffitiError n
     )
  => m (Atlas
        -> [(GlyphInfo, GlyphPos)]
        -> n (AllocatedRendering [TextTransform])
       )
makeDrawGlyphs = do
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
        (ps, uvs) <- UV.unzip <$> stringTris' atlas glyphs
        bufferGeometry position pbuf ps
        bufferGeometry uv uvbuf uvs
        glBindVertexArray 0

        let draw ts wsz = do
                let (mv, multVal) = transformToUniforms ts
                glUseProgram prog
                let pj = orthoProjection wsz
                updateUniform prog pjU pj
                updateUniform prog mvU mv
                updateUniform prog multU multVal
                updateUniform prog texU (0 :: Int)
                glBindVertexArray vao
                withBoundTextures [atlasTexture atlas] $ do
                    drawVAO prog vao GL_TRIANGLES (fromIntegral $ UV.length ps)
                    glBindVertexArray 0
            release = do
                withArray [pbuf, uvbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
            (tl, br) = boundingBox ps
            size = br - tl
        return AllocatedRendering {
            arDraw = draw,
            arRelease = release,
            arSize = round <$> size
          }

-- | The GPU code to finalize the position of glyphs onscreen.
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

-- | The GPU code to composite the recoloured glyph into the output image.
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

-- | Geometrically transform the text.
data SpatialTransform = SpatialTransformTranslate (V2 Float)
                      -- ^ Shift the text horizontally or vertically.
                      | SpatialTransformScale (V2 Float)
                      -- ^ Resize the text.
                      | SpatialTransformRotate Float
                      -- ^ Enlarge the text.
                      | SpatialTransformSkew Float
                      -- ^ Skew the text, approximating italics (or rather obliques).
                      | SpatialTransform (M44 Float)
                      -- ^ Apply an arbitrary matrix transform to the text.

-- | Modify the rendered text.
data TextTransform = TextTransformMultiply (V4 Float)
                   -- ^ Adjust the colour of the rendered text.
                   | TextTransformSpatial SpatialTransform
                   -- ^ Adjust the position of the rendered text.

-- | Convert the `TextTransform`s into data that can be sent to the GPU.
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
                SpatialTransformSkew x ->
                  mv !*! mat4SkewXbyY x
                SpatialTransform mat -> mv !*! mat
          in (mv1, clr)

-- | Shift the text horizontally or vertically.
move :: Float -> Float -> TextTransform
move x y =
  TextTransformSpatial
  $ SpatialTransformTranslate
  $ V2 x y

-- | Resize the text.
scale :: Float -> Float -> TextTransform
scale x y =
  TextTransformSpatial
  $ SpatialTransformScale
  $ V2 x y

-- | Rotate the text.
rotate :: Float -> TextTransform
rotate =
  TextTransformSpatial
  . SpatialTransformRotate

skew :: Float -> TextTransform
skew = TextTransformSpatial . SpatialTransformSkew

matrix :: M44 Float -> TextTransform
matrix = TextTransformSpatial . SpatialTransform

-- | Recolour the text.
color :: Float -> Float -> Float -> Float -> TextTransform
color r g b a =
  TextTransformMultiply
  $ V4 r g b a

-- | Make the text semi-transparant.
alpha :: Float -> TextTransform
alpha =
  TextTransformMultiply
  . V4 1 1 1


instance Layout [TextTransform] where
  translate ts (V2 x y) = ts ++ [move x y]

-- | Utility for calling OpenGL APIs in a error monad.
liftGL
  :: ( MonadIO m
     , MonadError TypograffitiError m
     )
  => m (Either String a)
  -> m a
liftGL n = do
  let lft = liftEither . first TypograffitiErrorGL
  n >>= lft
