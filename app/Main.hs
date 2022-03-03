{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad          (unless)
import           Control.Monad.Except   (runExceptT, MonadError)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Function          (fix)
import           Graphics.GL
import           SDL                    hiding (rotate)
import           System.FilePath        ((</>))

import           System.Environment

import           Typograffiti


myTextStuff
  :: ( MonadIO m
     , MonadError TypograffitiError m
     )
  => String -> Window -> m ()
myTextStuff text w = do
  let ttfName = "assets" </> "Lora-Regular.ttf"
  store <- newFontStoreForCharset (get $ windowSize w) text True
  RenderedText draw size <-
    getTextRendering
      store
      ttfName
      (GlyphSizeInPixels 16 16)
      text
  liftIO $ print ("text size", size)

  fix $ \loop -> do
    events <- fmap eventPayload
      <$> pollEvents

    glClearColor 0 0 0 1
    glClear GL_COLOR_BUFFER_BIT

    (V2 dw dh) <- glGetDrawableSize w
    glViewport 0 0 (fromIntegral dw) (fromIntegral dh)

    draw [move 20 32, rotate (pi / 4), color 1 0 1 1, alpha 0.5]

    glSwapWindow w
    unless (QuitEvent `elem` events) loop


main :: IO ()
main = do
  SDL.initializeAll

  args <- getArgs
  text <- case args of
    [] -> return $ unlines
      [ "Hey there!"
      , "This is a test of the emergency word system."
      , "Quite at any time."
      ]
    [path] -> readFile path
    _ -> return "Usage: typograffiti-exe [FILE]"

  let openGL = defaultOpenGL
        { glProfile = Core Debug 3 3 }
      wcfg = defaultWindow
        { windowInitialSize     = V2 640 480
        , windowGraphicsContext = OpenGLContext openGL
        , windowResizable       = True
        }

  w <- createWindow "Typograffiti" wcfg
  _ <- glCreateContext w

  runExceptT (myTextStuff text w)
    >>= either (fail . show) return
