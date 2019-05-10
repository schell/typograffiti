-- |
-- Module:     Typograffiti.Atlas
-- Copyright:  (c) 2019 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides a font-character atlas to use in font rendering with
-- a pluggable backend.
--
module Typograffiti.Atlas where

import           Data.IntMap                                       (IntMap)
import           Linear

import           Typograffiti.Glyph


--------------------------------------------------------------------------------
-- Atlas
--------------------------------------------------------------------------------


-- | An atlas holds a texture/bitmap that contains glyphs, as well as a map of
-- those glyphs measurements.
data Atlas tex rs
  = Atlas
  { atlasTexture     :: tex
  -- ^ The rasterized glyphs themselves
  , atlasResources   :: rs
  -- ^ Any other resources needed by the atlas
  , atlasTextureSize :: V2 Int
  -- ^ The size of the texture in pixels
  , atlasMetrics     :: IntMap GlyphMetrics
  -- ^ A mapping of glyph index to its metrics
  , atlasGlyphSize   :: GlyphSize
  -- ^ The maximum width of any glyphs in the atlas
  , atlasFilePath    :: FilePath
  -- ^ The location this texture was loaded from
  }


--emptyAtlas :: FT_Library -> FT_Face -> GLuint -> Atlas
--emptyAtlas lib fce t = Atlas t 0 lib fce mempty (GlyphSizeInPixels 0 0) ""


-- | A helper type for keeping track of rasterized glyphs.
data AtlasMeasure
  = AM
  { amWH      :: V2 Int
  , amXY      :: V2 Int
  , rowHeight :: Int
  } deriving (Show, Eq)


emptyAM :: AtlasMeasure
emptyAM = AM 0 (V2 1 1) 0


-- | The amount of spacing between glyphs rendered into the atlas's texture.
spacing :: Int
spacing = 1


-- | A string containing all standard ASCII characters.
-- This is often passed as the 'String' parameter in 'allocAtlas'.
asciiChars :: String
asciiChars = map toEnum [32..126]
