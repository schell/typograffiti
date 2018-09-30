{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad          (unless)
import           Control.Monad.Except   (MonadError, liftEither,
                                         runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Bifunctor         (first)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B8
import           Data.Function          (fix)
import qualified Data.Vector.Unboxed    as UV
import           Foreign.Marshal.Array
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


-- TODO: Include a default Cache.
-- That allows translation, scale, rotation and color.


instance Layout (V2 Float) where
  translate = (+)


makeAllocateWord
  :: ( MonadIO m
     , MonadError TypograffitiError m
     )
  => Window
  -> m (Atlas -> String -> m (AllocatedRendering (V2 Float) m))
makeAllocateWord window = do
  -- Compile our shader program
  let position = 0
      uv       = 1
      liftGL   = liftEither . first TypograffitiErrorGL
  vert <- liftGL =<< compileOGLShader vertexShader GL_VERTEX_SHADER
  frag <- liftGL =<< compileOGLShader fragmentShader GL_FRAGMENT_SHADER
  prog <- liftGL =<< compileOGLProgram
    [ ("position", fromIntegral position)
    , ("uv", fromIntegral uv)
    ]
    [vert, frag]
  glUseProgram prog
  -- Get our uniform locations
  projection <- getUniformLocation prog "projection"
  modelview  <- getUniformLocation prog "modelview"
  tex        <- getUniformLocation prog "tex"
  -- Return a function that will generate new words
  return $ \atlas string -> do
    liftIO $ putStrLn $ unwords ["Allocating", string]
    vao   <- newBoundVAO
    pbuf  <- newBuffer
    uvbuf <- newBuffer
    -- Generate our string geometry
    geom <- stringTris atlas True string
    let (ps, uvs) = UV.unzip geom
    -- Buffer the geometry into our attributes
    bufferGeometry position pbuf  ps
    bufferGeometry uv       uvbuf uvs
    glBindVertexArray 0

    let draw (V2 x y) = do
          liftIO $ pPrint (string, V2 x y)
          glUseProgram prog
          wsz <- get (windowSize window)
          let pj :: M44 Float = orthoProjection wsz
              mv :: M44 Float = mat4Translate (V3 x y 0)
          updateUniform prog projection pj
          updateUniform prog modelview  mv
          updateUniform prog tex (0 :: Int)
          glBindVertexArray vao
          withBoundTextures [atlasTexture atlas] $ do
            drawVAO
              prog
              vao
              GL_TRIANGLES
              (fromIntegral $ UV.length ps)
            glBindVertexArray 0
        release = liftIO $ do
          withArray [pbuf, uvbuf] $ glDeleteBuffers 2
          withArray [vao] $ glDeleteVertexArrays 1
        (tl, br) = boundingBox ps
        size = br - tl
    return AllocatedRendering
      { arDraw    = draw
      , arRelease = release
      , arSize    = round <$> size
      }


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

  e <- runExceptT $ do
    -- Get the atlas
    atlas <- allocAtlas
      ttfName
      (GlyphSizeInPixels 16 16)
      asciiChars

    allocWord <- makeAllocateWord w

    (draw, _, cache) <-
      loadText
        allocWord
        atlas
        mempty
        $ unlines
            [ "Hey there!"
            , "This is a test of the emergency word system."
            , "Quit at any time."
            ]

    -- Forever loop, drawing stuff
    fix $ \loop -> do

      events <- fmap eventPayload
        <$> pollEvents

      glClearColor 0 0 0 1
      glClear GL_COLOR_BUFFER_BIT

      (V2 dw dh) <- glGetDrawableSize w
      glViewport 0 0 (fromIntegral dw) (fromIntegral dh)

      draw $ V2 10 32
      glSwapWindow w

      unless (QuitEvent `elem` events) loop
    _ <- unloadMissingWords cache ""
    return ()
  either (fail . show) return e
