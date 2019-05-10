-- |
-- Module:     Typograffiti
-- Copyright:  (c) 2019 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@formation.ai>
--
-- This module provides the abstract functionality of a cached font atlas.
module Typograffiti
  (
  -- * $glyphs
    GlyphSize (..)
  , CharSize (..)
  , charGlyphAction
  -- * $atlas
  , Atlas (..)
  , asciiChars
  -- * $cache
  , WordCache (..)
  , AllocatedRendering (..)
  , loadWords
  , unloadMissingWords
  -- * $store
  , Store
  , RenderedGlyphs (..)
  , GlyphRenderingData (..)
  , getRendering
  -- * Transforming allocated renderings
  , Transform (..)
  , move
  , moveV2
  , scale
  , scaleV2
  , rotate
  ) where

import           Typograffiti.Atlas
import           Typograffiti.Cache
import           Typograffiti.Glyph
import           Typograffiti.Store
import           Typograffiti.Transform

-- | $glyphs

-- | $atlas
-- Typograffiti is in plain terms a cache of caches. Its core is the `Atlas`,
-- which is a collection of rasterized glyphs. These modules don't make any
-- assumptions as to what a glyph really is, though, which means you can use
-- Typograffiti for more than just rendering text. Indeed Typograffiti is great
-- for rendering anything that can be represented by contiguous strings. For
-- example - in tile-based games we often see the same formations again and again
-- where tiles repeat a given pattern. If these patterns can be recognized  and
-- broken up into contiguous, two dimensional lists, then Typograffiti can cache
-- the renderings of these patterns for you, greatly improving your rendering
-- framerate.

-- | To keep things as general as possible this package abstracts out two
-- important concepts - rasterization and the rendering itself. Most low level
-- functions will take rasterization or rendering functions as arguments and
-- the low level types will have type variables representing the details of these
-- abstractions.

-- | If you simply want to use Typograffiti to display TTF fonts without writing
-- your own rasterizer or rendering functions I suggest you use the
-- typograffiti-freetype package (which provides freetype glyph rasterization)
-- along with either typograffiti-sdl or typograffiti-gl (which each provide
-- rendering services).

-- | $cache Collections of rasterized strings

-- | $store Collections of WordCaches for each file at a certain size
