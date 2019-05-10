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

import           Typograffiti.GL


myTextStuff
  :: ( MonadIO m
     , MonadError String m
     )
  => Window -> m ()
myTextStuff w = do
  let ttfName = "assets" </> "Lora-Regular.ttf"
  store <- newDefaultFontStore (get $ windowSize w)
  RenderedGlyphs draw size <-
    getTextRendering
      store
      ttfName
      (GlyphSizeInPixels 16 16)
      $ unlines
          [ "Hey there!"
          , "This is a test of the emergency word system."
          , "Quit at any time."
          ]
  liftIO $ print ("text size" :: String, size)

  fix $ \loop -> do
    events <- fmap eventPayload
      <$> pollEvents

    glClearColor 0 0 0 1
    glClear GL_COLOR_BUFFER_BIT

    (V2 dw dh) <- glGetDrawableSize w
    glViewport 0 0 (fromIntegral dw) (fromIntegral dh)

    draw [move 20 32, rotate (pi / 4), color 1 0 1 1, alpha 0.5]
    --draw [move 100 100, color 1 1 1 1, scale 2 2]
    draw [move 100 100]

    glSwapWindow w
    unless (QuitEvent `elem` events) loop


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

  runExceptT (myTextStuff w)
    >>= either (fail . show) return
