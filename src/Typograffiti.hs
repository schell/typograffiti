-- |
-- Module:     Typograffiti
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides easy freetype2-based font rendering with a nice
-- Haskell interface.
module Typograffiti
  (
  -- * Some simple default text rendering operations
    RenderedText (..)
  , TextRenderingData (..)
  , FontStore
  , newDefaultFontStore
  , newFontStoreForCharset
  , getTextRendering
  -- * Transforming rendered text
  , TextTransform (..)
  -- TODO Vector variants of the transformation helpers.
  -- i.e. moveV2, scaleV2, colorV4
  , move
  , scale
  , rotate
  , color
  , alpha
  , Layout (..)
  -- * Getting low
  , allocAtlas
  , loadText
  , unloadMissingWords
  , stringTris
  , makeDefaultAllocateWord
  , asciiChars
  -- * Types
  , GlyphSize (..)
  , CharSize (..)
  , Atlas (..)
  , WordCache (..)
  , AllocatedRendering (..)
  -- * Errors
  , TypograffitiError (..)
  ) where

import           Typograffiti.Atlas
import           Typograffiti.Cache
import           Typograffiti.Glyph
import           Typograffiti.Store
