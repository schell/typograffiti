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
    args <- getArgs
    let text = pack $ case args of
          [] -> unlines [
            "Decoder Ring Theatre brings you the continuing adventures",
            "of Canada's greatest superhero, that scourage of the underworld,",
            "hunter of those who pray upon the innocent,",
            "that marvelous masked mystery man",
            "known only as The Red Panda!",
            "",
            "The Red Panda, masked crucader for justice, hides his secret identity",
            "as one of the city's wealthiest men in his neverending battle",
            "against crime & corruption. Only his trust driver, Kit Baxter",
            "who joins him in the guise of The Flying Squirrel,",
            "knows who wears the mask of The Red Panda!"]
          _ -> unwords args
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

            liftIO $ arDraw drawText' [
                TextTransformSpatial $ SpatialTransformTranslate $ fromIntegral 10
              ] (fromIntegral <$> sz)

            liftIO $ glSwapWindow w
            unless (QuitEvent `elem` events) loop
    return ()
