{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Module:     Typograffiti.Cache
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides a method of caching rendererd text, making it suitable
-- for interactive rendering. You can use the defaultCache or provide your own.
--
module Typograffiti.Cache where

import           Control.Monad          (foldM)
import           Control.Monad.Except   (MonadError (..), liftEither,
                                         runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Bifunctor         (first)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B8
import qualified Data.IntMap            as IM
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Vector.Unboxed    as UV
import           Foreign.Marshal.Array
import           Graphics.GL
import           Linear

import           Typograffiti.Atlas
import           Typograffiti.GL
import           Typograffiti.Glyph

data AllocatedRendering = AllocatedRendering
  { arDraw    :: [TextTransform] -> V2 CInt -> IO ()
    -- ^ Draw the text with some transformation in some monad.
  , arRelease :: IO ()
    -- ^ Release the allocated draw function in some monad.
  , arSize    :: V2 Int
    -- ^ The size (in pixels) of the drawn text.
  }

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
        (ps, uvs) <- unzip <$> stringTris' atlas glyphs
        bufferGeometry position pbuf $ UV.fromList ps
        bufferGeometry uv uvbuf $ UV.fromList uvs
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
                    drawVAO prog vao GL_TRIANGLES (fromIntegral $ length ps)
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

move :: Float -> Float -> TextTransform
move x y =
  TextTransformSpatial
  $ SpatialTransformTranslate
  $ V2 x y


scale :: Float -> Float -> TextTransform
scale x y =
  TextTransformSpatial
  $ SpatialTransformScale
  $ V2 x y


rotate :: Float -> TextTransform
rotate =
  TextTransformSpatial
  . SpatialTransformRotate


color :: Float -> Float -> Float -> Float -> TextTransform
color r g b a =
  TextTransformMultiply
  $ V4 r g b a


alpha :: Float -> TextTransform
alpha =
  TextTransformMultiply
  . V4 1 1 1


instance Layout [TextTransform] where
  translate ts (V2 x y) = ts ++ [move x y]


liftGL
  :: ( MonadIO m
     , MonadError TypograffitiError m
     )
  => m (Either String a)
  -> m a
liftGL n = do
  let lft = liftEither . first TypograffitiErrorGL
  n >>= lft
