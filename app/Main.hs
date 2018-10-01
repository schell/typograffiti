{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad        (unless)
import           Control.Monad.Except (runExceptT)
import           Data.Function        (fix)
import           Graphics.GL
import           SDL                  hiding (rotate)
import           System.FilePath      ((</>))

import           Typograffiti


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

    allocWord <- makeDefaultAllocateWord (get $ windowSize w)

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

      draw [move 20 32, rotate (pi / 4), color 1 0 1 1, alpha 0.5]

      glSwapWindow w
      unless (QuitEvent `elem` events) loop
    _ <- unloadMissingWords cache ""
    return ()
  either (fail . show) return e
