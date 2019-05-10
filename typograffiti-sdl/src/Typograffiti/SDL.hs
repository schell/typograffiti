-- | Provides easy freetype2 and SDL based font rendering with a nice Haskell
-- interface.
module Typograffiti.SDL
  (
    module Typograffiti
  , newDefaultFontStore
  , newFontStoreWithGlyphs
  , getTextRendering
  , color
  , colorV4
  , alpha
  , allocAtlas
  , loadText
  , makeDefaultAllocateWord
  ) where

import           Typograffiti
import           Typograffiti.SDL.Atlas
import           Typograffiti.SDL.Cache
import           Typograffiti.SDL.Store
import           Typograffiti.SDL.Transform
