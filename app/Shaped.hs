{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Typograffiti (makeDrawGlyphs, allocAtlas, TextTransform(..),
                     AllocatedRendering(..), SpatialTransform(..))
import Typograffiti.Atlas (glyphRetriever)
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import SDL hiding (rotate, trace)
import Graphics.GL.Core32

import Data.Function (fix)
import Data.Text.Lazy (pack)
import Control.Monad (unless, forM)

import qualified Data.IntSet as IS
import Data.Int (Int32)
import Data.Text.Glyphize (GlyphInfo(..), GlyphPos(..))
import FreeType.Core.Base (ft_With_FreeType, ft_With_Face)
import Debug.Trace (trace)

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
    let (fontfile, ppemX, ppemY, infile) = case args of
            (fontfile:ppem:infile:_)
                | (ppemX, ',':ppemY) <- break (== ',') ppem ->
                    (fontfile, read ppemX, read ppemY, infile)
                | otherwise -> (fontfile, read ppem, read ppem, infile)
            _ -> trace "USAGE: draw-shaped FONTFILE PPEM FILE"
                (ttfName, 15, 15, "shaped.txt")
    text <- read <$> readFile infile :: IO [(Int32,Int32,[(GlyphInfo,GlyphPos)])]
    let glyphs = IS.fromList [fromIntegral $ codepoint info
            | (_, _, glyphs) <- text, (info, _) <- glyphs]

    atlas' <- ft_With_FreeType $ \ft -> ft_With_Face ft fontfile 0 $ \font -> do
        let font' = glyphRetriever font
        runExceptT $ allocAtlas font' (map toEnum $ IS.toList glyphs) (ppemX, ppemY)

    err <- runExceptT $ do
        drawGlyphs <- makeDrawGlyphs
        atlas <- liftEither atlas'
        fix $ \loop -> do
            events <- fmap eventPayload <$> pollEvents
            liftIO $ glClearColor 0 0 0 1
            liftIO $ glClear GL_COLOR_BUFFER_BIT

            sz@(V2 dw dh) <- liftIO $ glGetDrawableSize w
            liftIO $ glViewport 0 0 (fromIntegral dw) (fromIntegral dh)

            forM text $ \(x, y, para) -> do
                sprite <- drawGlyphs atlas para
                liftIO $ arDraw sprite [
                    TextTransformSpatial $ SpatialTransformTranslate $
                        fromIntegral <$> V2 x y
                  ] (fromIntegral <$> sz)

            liftIO $ glSwapWindow w
            unless (QuitEvent `elem` events) loop
    print err
    return ()
