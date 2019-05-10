{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.STM (atomically, putTMVar, takeTMVar)
import           Control.Monad          (unless, foldM_)
import           Control.Monad.Except   (MonadError, runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Function          (fix)
import qualified Data.Map               as M
import           Linear                 (V2 (..), V4 (..))
import           SDL                    (Renderer, ($=))
import qualified SDL
import           System.FilePath        ((</>))

import           Typograffiti.SDL
import           Typograffiti.Store


myTextStuff
  :: ( MonadIO m
     , MonadError String m
     )
  => Renderer
  -> m ()
myTextStuff r = do
  let ttfName = "assets" </> "Lora-Regular.ttf"
      glyphSz = GlyphSizeInPixels 16 16
  store <- newDefaultFontStore r
  RenderedGlyphs draw size <-
    getTextRendering
      r
      store
      ttfName
      glyphSz
      $ unlines
          [ "Hey there!"
          , "This is a test of the emergency word system."
          , "Quit at any time."
          ]

  liftIO $ print ("text size" :: String, size)

  fix $ \loop -> do
    events <- fmap SDL.eventPayload
      <$> SDL.pollEvents

    SDL.rendererDrawColor r $= V4 175 175 175 255
    SDL.clear r

    --s@(GlyphRenderingData _ dict _) <-
    --  liftIO
    --    $ atomically
    --    $ takeTMVar
    --    $ unStore store

    --case M.lookup (ttfName, glyphSz) dict of
    --  Nothing -> return ()
    --  Just (Dictionary atlas cache) -> do
    --    SDL.copy
    --      r
    --      (atlasTexture atlas)
    --      Nothing
    --      $ Just
    --      $ SDL.Rectangle
    --         0
    --         $ fromIntegral
    --           <$> atlasTextureSize atlas

    --    let V2 _ startingY = atlasTextureSize atlas
    --        renderTex y ar = do
    --          case (arTextures ar, arSizes ar) of
    --            (tex:_, sz@(V2 _ szy):_) -> do
    --              SDL.copy
    --                r
    --                tex
    --                Nothing
    --                $ Just
    --                $ SDL.Rectangle
    --                    (SDL.P $ fromIntegral <$> V2 0 y)
    --                    $ fromIntegral
    --                      <$> sz
    --              return $ y + szy
    --            (_, _) -> return y
    --    foldM_
    --      renderTex
    --      startingY
    --      $ M.elems
    --      $ unWordCache cache

    --liftIO
    --  $ atomically
    --  $ putTMVar
    --      (unStore store)
    --      s

    draw [move 100 100, color 1.0 0 0 1.0]
    draw [move 100 120, color 1.0 1.0 1.0 1.0]

    SDL.present r
    unless (SDL.QuitEvent `elem` events) loop


main :: IO ()
main = do
  SDL.initializeAll

  let wcfg = SDL.defaultWindow
        { SDL.windowInitialSize = V2 640 480 }
      rcfg = SDL.defaultRenderer
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer }

  w <- SDL.createWindow "Typograffiti SDL" wcfg
  r <- SDL.createRenderer w (-1) rcfg

  SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend

  runExceptT (myTextStuff r)
    >>= either (fail . show) return
