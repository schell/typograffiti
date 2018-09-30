{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:     Gelatin.FreeType2
-- Copyright:  (c) 2017 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides easy freetype2 font rendering using gelatin's
-- graphics primitives.
--
module Typograffiti
  ( allocAtlas
  , GlyphSize (..)
  , CharSize (..)
  , TypograffitiError (..)
  , Atlas (..)
  , WordCache (..)
  , AllocatedRendering (..)
  , Layout (..)
  , asciiChars
  , stringTris
  , loadText
  , unloadMissingWords
  ) where

import           Typograffiti.Atlas
import           Typograffiti.Cache
import           Typograffiti.Glyph
