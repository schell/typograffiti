-- |
-- Module:     Typograffiti
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides easy freetype2-based font rendering with a nice
-- Haskell interface.
module Typograffiti(
    TypograffitiError(..),
    allocAtlas, freeAtlas, stringTris, Atlas(..), GlyphMetrics(..),
    makeDrawGlyphs, AllocatedRendering(..), Layout(..),
    SpatialTransform(..), TextTransform(..), move, scale, rotate, color, alpha,
    withFontStore, newFontStore, FontStore(..), Font(..),
    makeDrawTextIndentedCached, makeDrawTextCached,
        makeDrawAsciiIndentedCached, makeDrawAsciiCached,
    makeDrawTextIndented, makeDrawTextIndented', makeDrawText, makeDrawText',
        makeDrawAsciiIndented, makeDrawAsciiIndented', makeDrawAscii, makeDrawAscii'
) where

import Typograffiti.Atlas
import Typograffiti.Cache
import Typograffiti.Store
import Typograffiti.Text
