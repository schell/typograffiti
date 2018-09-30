{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Module:     Typograffiti.Cache
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides a method of caching rendererd text, making it suitable
-- for interactive rendering. You can use the defaultCache or provide your own.
--
module Typograffiti.Cache where

import           Control.Monad          (foldM)
import qualified Data.IntMap            as IM
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Linear

import           Typograffiti.Atlas
import           Typograffiti.Glyph

--data SpatialTransform = SpatialTransformTranslate (V2 Float)
--                      | SpatialTransformScale (V2 Float)
--                      | SpatialTransformRotate Float
--
--
--data FontTransform = FontTransformAlpha Float
--                   | FontTransformMultiply (V4 Float)
--                   | FontTransformReplaceRed (V4 Float)
--                   | FontTransformSpatial SpatialTransform


-- | Generic operations for text layout.
class Layout t where
  translate :: t -> V2 Float -> t


-- | Holds an allocated draw function for some amount of text. The function
-- takes one parameter that can be used to transform the text in various ways.
-- This type is generic and can be used to take advantage of your own font
-- rendering shaders.
data AllocatedRendering t m = AllocatedRendering
  { arDraw    :: t -> m ()
    -- ^ Draw the text with some transformation in some monad.
  , arRelease :: m ()
    -- ^ Release the allocated draw function in some monad.
  , arSize    :: V2 Int
    -- ^ The size (in pixels) of the drawn text.
  }


newtype WordCache t m = WordCache
  { unWordCache :: Map String (AllocatedRendering t m) }
  deriving (Semigroup, Monoid)


-- | Load a string of words into the WordCache.
loadWords
  :: Monad m
  => (Atlas -> String -> m (AllocatedRendering t m))
  -- ^ Operation used to allocate a word.
  -> Atlas
  -- ^ The character atlas that holds our letters, which is used to generate
  -- the word geometry.
  -> WordCache t m
  -- ^ The atlas to load the words into.
  -> String
  -- ^ The string of words to load, with each word separated by spaces.
  -> m (WordCache t m)
loadWords f atlas (WordCache cache) str = do
  wm <- foldM loadWord cache (words str)
  return $ WordCache wm
  where loadWord wm word
          | M.member word wm = return wm
          | otherwise = do
              w <- f atlas word
              return $ M.insert word w wm


-- | Unload any words from the cache that are not contained in the source string.
unloadMissingWords
  :: Monad m
  => WordCache t m
  -- ^ The WordCache to unload words from.
  -> String
  -- ^ The source string.
  -> m (WordCache t m)
unloadMissingWords (WordCache cache) str = do
  let ws      = M.fromList $ zip (words str) (repeat ())
      missing = M.difference cache ws
      retain  = M.difference cache missing
  sequence_ $ arRelease <$> missing
  return $ WordCache retain


-- | Constructs a 'Renderer2' from the given color and string. The 'WordMap'
-- record of the given 'Atlas' is used to construct the string geometry, greatly
-- improving performance and allowing longer strings to be compiled and renderered
-- in real time. To create a new 'Atlas' see 'allocAtlas'.
--
-- Note that since word geometries are stored in the 'Atlas' 'WordMap' and multiple
-- renderers can reference the same 'Atlas', the returned 'Renderer2' contains a
-- clean up operation that does nothing. It is expected that the programmer
-- will call 'freeAtlas' manually when the 'Atlas' is no longer needed.
loadText
  :: forall m t. (Monad m, Layout t)
  => (Atlas -> String -> m (AllocatedRendering t m))
  -- ^ Operation used to allocate a word.
  -> Atlas
  -- ^ The character atlas that holds our letters.
  -> WordCache t m
  -- ^ The WordCache to load AllocatedRenderings into.
  -> String
  -- ^ The string to render.
  -- This string may contain newlines, which will be respected.
  -> m (t -> m (), V2 Int, WordCache t m)
  -- ^ Returns a function for rendering the text, the size of the text and the
  -- new WordCache with the allocated renderings of the text.
loadText f atlas wc@(WordCache cache) str = do
  wc1@(WordCache cache1) <- loadWords f atlas wc str
  let glyphw  = round $ pixelWidth $ atlasGlyphSize atlas
      spacew  :: Int
      spacew  = fromMaybe glyphw $ do
        metrcs <- IM.lookup (fromEnum ' ') $ atlasMetrics atlas
        let V2 x _ = glyphAdvance metrcs
        return x
      glyphh = pixelHeight $ atlasGlyphSize atlas
      spaceh = round glyphh
      isWhiteSpace c = c == ' ' || c == '\n' || c == '\t'
      renderWord :: t -> V2 Int -> String -> m ()
      renderWord _ _ ""       = return ()
      renderWord t (V2 _ y) ('\n':cs) = renderWord t (V2 0 (y + spaceh)) cs
      renderWord t (V2 x y) (' ':cs)  = renderWord t (V2 (x + spacew) y) cs
      renderWord t v@(V2 x y) cs               = do
        let word = takeWhile (not . isWhiteSpace) cs
            rest = drop (length word) cs
        case M.lookup word cache1 of
          Nothing -> renderWord t v rest
          Just ar -> do
            let t1 = translate t $ fromIntegral <$> v
                V2 w _ = arSize ar
                pen = V2 (x + fromIntegral w) y
            arDraw ar t1
            renderWord t pen rest
      rr t = renderWord t 0 str
      measureString :: (V2 Int, V2 Int) -> String -> (V2 Int, V2 Int)
      measureString (V2 x y, V2 w h) ""        = (V2 x y, V2 w h)
      measureString (V2 x y, V2 w _) (' ':cs)  =
        let nx = x + spacew in measureString (V2 nx y, V2 (max w nx) y) cs
      measureString (V2 x y, V2 w h) ('\n':cs) =
        let ny = y + spaceh in measureString (V2 x ny, V2 w (max h ny)) cs
      measureString (V2 x y, V2 w h) cs        =
        let word = takeWhile (not . isWhiteSpace) cs
            rest = drop (length word) cs
            n    = case M.lookup word cache of
                     Nothing -> (V2 x y, V2 w h)
                     Just ar -> let V2 ww _ = arSize ar
                                    nx      = x + ww
                                in (V2 nx y, V2 (max w nx) y)
        in measureString n rest
      V2 szw szh = snd $ measureString (0,0) str
  return (rr, V2 szw (max spaceh szh), wc1)
