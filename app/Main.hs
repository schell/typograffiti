{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Monad          (forever)
import           Control.Monad.Except   (runExceptT, withExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B8
import qualified Data.Vector.Unboxed    as UV
import           Graphics.GL
import           SDL
import           System.FilePath        ((</>))
import           Typograffiti
import           Typograffiti.GL


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
  , "  gl_Position = projection * modelview * vec4(position, 0.0, 0.1);"
  , "}"
  ]


fragmentShader :: ByteString
fragmentShader = B8.pack $ unlines
  [ "#version 330 core"
  , "in vec2 fuv;"
  , "out vec4 fcolor;"
  , "uniform sampler2D tex;"
  , "void main () {"
  , "  fcolor = texture(tex, fuv);"
  , "}"
  ]


main :: IO ()
main = do
  SDL.initializeAll

  let openGL = defaultOpenGL
        { glProfile = Core Debug 3 3 }
      wcfg = defaultWindow
        { windowInitialSize = V2 640 480
        , windowOpenGL = Just openGL
        }

  w <- createWindow "Typograffiti" wcfg
  _ <- glCreateContext w
  let ttfName = "assets" </> "Neuton-Regular.ttf"

  (either fail return =<<) . runExceptT $ do
    -- Get the atlas
    atlas <- withExceptT show
      $ allocAtlas ttfName (PixelSize 16 16) asciiChars
    -- Compile our shader program
    let position = 0
        uv       = 1
    vert <- compileOGLShader vertexShader GL_VERTEX_SHADER
    frag <- compileOGLShader fragmentShader GL_FRAGMENT_SHADER
    prog <- compileOGLProgram
      [ ("position", fromIntegral position)
      , ("uv", fromIntegral uv)
      ]
      [vert, frag]
    glUseProgram prog
    -- Get our uniform locations
    projection <- getUniformLocation prog "projection"
    modelview  <- getUniformLocation prog "modelview"
    tex        <- getUniformLocation prog "tex"
    -- Generate our string geometry
    geom <- withExceptT show
      $ stringTris atlas True "Hi there"
    let (ps, uvs) = UV.unzip geom
    -- Buffer the geometry into our attributes
    textVao <- withVAO $ \vao -> do
      withBuffers 2 $ \[pbuf, uvbuf] -> do
        bufferGeometry position pbuf  ps
        bufferGeometry uv       uvbuf uvs
        return vao
    atlasVao <- withVAO $ \vao -> do
      withBuffers 2 $ \[pbuf, uvbuf] -> do
        let V2 w h = fromIntegral
              <$> atlasTextureSize atlas
        bufferGeometry position pbuf $ UV.fromList
          [ V2 0 0, V2 w 0, V2 w h
          , V2 0 0, V2 w h, V2 0 h
          ]
        bufferGeometry uv uvbuf $ UV.fromList
          [ V2 0 0, V2 1 0, V2 1 1
          , V2 0 0, V2 1 1, V2 0 1
          ]
        return vao

    -- Set our model view transform
    let mv :: M44 Float
        mv = mat4Translate (V3 10 100 0)
        mv2 :: M44 Float
        mv2 = mv !*! mat4Scale (V3 0.125 0.125 1)
    -- Forever loop, drawing stuff
    forever $ do
      _ <- pollEvents
      pj :: M44 Float <-
        orthoProjection <$> get (windowSize w)
      withBoundTextures [atlasTexture atlas] $ do
        updateUniform prog projection pj
        updateUniform prog modelview mv
        updateUniform prog tex (0 :: Int)
        drawVAO
          prog
          textVao
          GL_TRIANGLES
          (fromIntegral $ UV.length ps)

        updateUniform prog modelview mv2
        drawVAO
          prog
          atlasVao
          GL_TRIANGLES
          6
      glSwapWindow w
