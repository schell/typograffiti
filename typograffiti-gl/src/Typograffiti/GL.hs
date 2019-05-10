-- | Provides easy freetype2 and OpenGL based font rendering with a nice Haskell
-- interface.
module Typograffiti.GL
  (
    module Typograffiti
  , newDefaultFontStore
  , getTextRendering
  , color
  , colorV4
  , alpha
  , allocAtlas
  , loadText
  , makeDefaultAllocateWord
  ) where

import           Typograffiti

import           Typograffiti.GL.Atlas
import           Typograffiti.GL.Cache
import           Typograffiti.GL.Store
import           Typograffiti.GL.Transform
