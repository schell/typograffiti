{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Graphics.Text.Font.Render (makeDrawText', GlyphSize(..), TextTransform(..),
                                    AllocatedRendering(..), SpatialTransform(..))
import SDL hiding (rotate)
import Graphics.GL.Core32

import Data.Function (fix)
import Data.Text.Lazy (pack)
import Control.Monad (unless)

main :: IO ()
main = do
    SDL.initializeAll

    let openGL = defaultOpenGL { glProfile = Core Debug 3 3 }
        wcfg = defaultWindow {
            windowInitialSize = V2 640 480,
            windowGraphicsContext = OpenGLContext openGL,
            windowResizable = True
          }
    w <- createWindow "Typograffiti" wcfg
    _ <- glCreateContext w

    let ttfName = "assets/Lora-Regular.ttf"
    text <- pack <$> unwords <$> getArgs
    drawText <- makeDrawText' ttfName 0 (PixelSize 15 15) [] text
    drawText' <- drawText text

    fix $ \loop -> do
        events <- fmap eventPayload <$> pollEvents
        glClearColor 0 0 0 1
        glClear GL_COLOR_BUFFER_BIT

        sz@(V2 dw dh) <- glGetDrawableSize w
        glViewport 0 0 (fromIntegral dw) (fromIntegral dh)

        let offset = V2 0 $ fromIntegral dy
            V2 _ dy = arSize drawText'
        arDraw drawText' [
            TextTransformSpatial $ SpatialTransformTranslate offset
          ] sz

        glSwapWindow w
        unless (QuitEvent `elem` events) loop
