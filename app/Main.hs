{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Monad          (unless)
import           Control.Monad.Except   (runExceptT, withExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B8
import           Data.Function          (fix)
import qualified Data.Vector.Unboxed    as UV
import           Graphics.GL
import           SDL
import           System.FilePath        ((</>))
import           Text.Show.Pretty       (pPrint)

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
  , "  gl_Position = projection * modelview * vec4(position.xy, 0.0, 1.0);"
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


-- TODO: Word caching.
-- Somehow make it so it isn't bonded to one kind of
-- shader. It would be nice if users could write their own
-- shaders for this. At the same time, they shouldn't have to.


main :: IO ()
main = do
  SDL.initializeAll

  let openGL = defaultOpenGL
        { glProfile = Core Debug 3 3 }
      wcfg = defaultWindow
        { windowInitialSize = V2 640 480
        , windowOpenGL      = Just openGL
        , windowResizable   = True
        }

  w <- createWindow "Typograffiti" wcfg
  _ <- glCreateContext w
  let ttfName = "assets" </> "Lora-Regular.ttf"

  (either fail return =<<) . runExceptT $ do
    -- Get the atlas
    atlas <- withExceptT show
      $ allocAtlas
          ttfName
          (GlyphSizeInPixels 16 16)
          asciiChars
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
      $ stringTris atlas True "Typograffiti from your head to your feetee."
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
        mv = mat4Translate (V3 0 16 0)
        mv2 :: M44 Float
        mv2 = mv !*! mat4Translate (V3 0 16 0)
    -- Forever loop, drawing stuff
    fix $ \loop -> do

      events <- fmap eventPayload
        <$> pollEvents

      glClearColor 0 0 0 1
      glClear GL_COLOR_BUFFER_BIT

      dsz@(V2 dw dh) <- glGetDrawableSize w
      glViewport 0 0 (fromIntegral dw) (fromIntegral dh)

      wsz <- get (windowSize w)
      let pj :: M44 Float = orthoProjection wsz

      withBoundTextures [atlasTexture atlas] $ do
        updateUniform prog projection pj
        updateUniform prog modelview mv
        updateUniform prog tex (0 :: Int)
        drawVAO
          prog
          textVao
          GL_TRIANGLES
          (fromIntegral $ UV.length ps)

        updateUniform prog projection pj
        updateUniform prog modelview mv2
        drawVAO
          prog
          atlasVao
          GL_TRIANGLES
          6
      glSwapWindow w
      unless (any (== QuitEvent) events) loop
