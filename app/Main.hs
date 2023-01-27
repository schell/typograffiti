{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Typograffiti (makeDrawText', GlyphSize(..), TextTransform(..), txt,
                     SampleText(..), defaultSample, AllocatedRendering(..),
                     SpatialTransform(..))
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
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
    drawText <- makeDrawText' ttfName 0 (PixelSize 15 15) $ defaultSample { sampleText = text }
    runExceptT $ do
        drawText0 <- liftEither drawText
        drawText' <- drawText0 $ txt text

        fix $ \loop -> do
            events <- fmap eventPayload <$> pollEvents
            liftIO $ glClearColor 0 0 0 1
            liftIO $ glClear GL_COLOR_BUFFER_BIT

            sz@(V2 dw dh) <- liftIO $ glGetDrawableSize w
            liftIO $ glViewport 0 0 (fromIntegral dw) (fromIntegral dh)

            let offset = V2 0 $ fromIntegral dy
                V2 _ dy = arSize drawText'
            liftIO $ arDraw drawText' [
                TextTransformSpatial $ SpatialTransformTranslate $ fromIntegral dy
              ] (fromIntegral <$> sz)

            liftIO $ glSwapWindow w
            unless (QuitEvent `elem` events) loop
    return ()
