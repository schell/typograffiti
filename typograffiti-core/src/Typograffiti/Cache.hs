-- |
-- Module:     Typograffiti.Cache
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides a method of caching rendererd text, making it suitable
-- for fast, interactive rendering. You can use the defaultCache or provide your
-- own.
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Typograffiti.Cache where

import           Control.Monad          (foldM)
import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Linear                 (V2 (..))

import           Typograffiti.Atlas
import           Typograffiti.Glyph


--------------------------------------------------------------------------------
-- Allocating things and storing them
--------------------------------------------------------------------------------

-- | Holds an allocated draw function for some amount of text. The function
-- takes one parameter that can be used to transform the text in various ways.
data AllocatedRendering tfrm
  = AllocatedRendering
  { arDraw    :: tfrm -> IO ()
    -- ^ Draw the text with some transformation in some monad.
  , arRelease :: IO ()
    -- ^ Release the allocated draw function in some monad.
  , arSize    :: V2 Int
    -- ^ The size (in pixels) of the drawn text.
  }


-- | A map of lists of things (ie tiles, characters) to allocated renderings.
-- In the case of rasterizing fonts 'a' is a character.
-- In the case of rasterizing a spritesheet 'a' is a tile or frame.
newtype WordCache a tfrm
  = WordCache
  { unWordCache :: Map [a] (AllocatedRendering tfrm) }
  deriving (Semigroup, Monoid)


-- |
data TypograffitiError a e
  = TypograffitiErrorNoGlyphMetricsForGlyph a
  -- ^ The are no glyph metrics for this glyph. This probably means
  -- the character has not been loaded into the atlas.
  | TypograffitiError e
  deriving (Show, Eq)


-- | Load a list of "words" into the WordCache.
-- For example:
--
-- @preloadWords allocWord atlas cache $ words "this is my string to render"@
preloadWords
  :: ( MonadIO m
     , MonadError (TypograffitiError a e) m
     , Ord a
     )
  => (Atlas tex rs -> [a] -> m (AllocatedRendering tfrm))
  -- ^ Operation used to allocate a word.
  -> Atlas tex rs
  -- ^ The character atlas that holds our letters, which is used to generate
  -- the word geometry.
  -> WordCache a tfrm
  -- ^ The atlas to load the words into.
  -> [[a]]
  -- ^ The list of words to load
  -> m (WordCache a tfrm)
preloadWords f atlas (WordCache cache) =
  fmap WordCache
  . foldM loadWord cache
  where
    loadWord wm word
      | M.member word wm = return wm
      | otherwise = flip (M.insert word) wm <$> f atlas word


-- | Unload any words from the cache that are not contained in the source string.
unloadMissingWords
  :: ( MonadIO m
     , Ord a
     )
  => WordCache a tfrm
  -- ^ The WordCache to unload words from.
  -> [[a]]
  -- ^ The source words.
  -> m (WordCache a tfrm)
unloadMissingWords (WordCache cache) strs = do
  let ws      = M.fromList $ zip strs (repeat ())
      missing = M.difference cache ws
      retain  = M.difference cache missing
  liftIO
    $ sequence_
    $ arRelease <$> missing
  return
    $ WordCache retain


-- | To store all our rendering configuration data.
data RenderHelper tfrm a
  = RenderHelper
  { renderHelperTranslate :: tfrm -> V2 Float -> tfrm
  , renderHelperMkAction  :: a -> GlyphAction
  , renderHelperCache     :: WordCache a tfrm
  , renderHelperSpace     :: V2 Int
  }


renderWord
  :: Ord a
  => RenderHelper tfrm a
  -> tfrm
  -> V2 Int
  -> [a]
  -> IO ()
renderWord _ _ _ [] = return ()
renderWord
  helper@(RenderHelper translate mkAction cache (V2 spacew spaceh))
  t
  v@(V2 x y)
  gs@(c:cs) =

  case mkAction c of
    GlyphActionNewline -> renderWord helper t (V2 0 (y + spaceh)) cs
    GlyphActionSpace   -> renderWord helper t (V2 (x + spacew) y) cs
    GlyphActionRender  -> do
      let word = takeWhile (isRenderGlyph mkAction) gs
          rest = drop (length word) gs
      case M.lookup word $ unWordCache cache of
        Nothing -> renderWord helper t v rest
        Just ar -> do
          let t1 = translate t $ fromIntegral <$> v
              V2 w _ = arSize ar
              pen = V2 (x + fromIntegral w) y
          arDraw ar t1
          renderWord helper t pen rest


measureString
  :: Ord a
  => RenderHelper tfrm a
  -> (V2 Int, V2 Int)
  -> [a]
  -> (V2 Int, V2 Int)
measureString _ xywh [] = xywh
measureString
  helper@(RenderHelper _ mkAction cache (V2 spacew spaceh))
  (V2 x y, V2 w h)
  gs@(c:cs) =
    case mkAction c of
      GlyphActionSpace ->
        let nx = x + spacew
        in measureString helper (V2 nx y, V2 (max w nx) y) cs
      GlyphActionNewline ->
        let ny = y + spaceh
        in measureString helper (V2 x ny, V2 w (max h ny)) cs
      GlyphActionRender ->
        let word = takeWhile (isRenderGlyph mkAction) gs
            rest = drop (length word) gs
            n    = case M.lookup word $ unWordCache cache of
                    Nothing -> (V2 x y, V2 w h)
                    Just ar -> let V2 ww _ = arSize ar
                                   nx      = x + ww
                                in (V2 nx y, V2 (max w nx) y)
        in measureString helper n rest


-- | Load the given glyph string into the given WordCache using the given monadic
-- rendering and transform operations.
loadWords
  :: forall m e a tfrm tex rs.
     ( MonadIO m
     , MonadError (TypograffitiError a e) m
     , Ord a
     )
  => (tfrm -> V2 Float -> tfrm)
  -- ^ A pure function for translating a transform.
  -> (a -> GlyphAction)
  -- ^ A pure function to determine an action the renderer should take based on
  -- the current glyph. For strings this should be 'charGlyphAction'.
  -> (Atlas tex rs -> [a] -> m (AllocatedRendering tfrm))
  -- ^ Monadic operation used to allocate a word.
  -> Atlas tex rs
  -- ^ The character atlas that holds our letters.
  -> WordCache a tfrm
  -- ^ The WordCache to load AllocatedRenderings into.
  -> [a]
  -- ^ The string to render.
  -- This string may contain newlines, which will be respected.
  -> m (tfrm -> IO (), V2 Int, WordCache a tfrm)
  -- ^ Returns a function for rendering the text, the size of the text and the
  -- new WordCache with the allocated renderings of the text.
loadWords translate mkAction f atlas wc str = do
  -- preload the words into the word cache first
  wc1 <-
    preloadWords
      f
      atlas
      wc
      $ breakWords mkAction str

  let spacew = round $ pixelWidth $ atlasGlyphSize atlas
      glyphh = pixelHeight $ atlasGlyphSize atlas
      spaceh = round glyphh
      helper =
        RenderHelper
          translate
          mkAction
          wc1
          (V2 spacew spaceh)
      rr t = renderWord helper t 0 str
      V2 szw szh = snd $ measureString helper (0,0) str
  return (rr, V2 szw (max spaceh szh), wc1)
