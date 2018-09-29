module Typograffiti.Glyph where


import Linear


data GlyphSize = CharSize Float Float Int Int
               | PixelSize Int Int
               deriving (Show, Eq, Ord)


glyphWidth :: GlyphSize -> Float
glyphWidth (CharSize x y _ _) = if x == 0 then y else x
glyphWidth (PixelSize x y)    = fromIntegral $ if x == 0 then y else x


glyphHeight :: GlyphSize -> Float
glyphHeight (CharSize x y _ _) = if y == 0 then x else y
glyphHeight (PixelSize x y)    = fromIntegral $ if y == 0 then x else y


-- | https://www.freetype.org/freetype2/docs/tutorial/step2.html
data GlyphMetrics = GlyphMetrics
  { glyphTexBB       :: (V2 Int, V2 Int)
  , glyphTexSize     :: V2 Int
  , glyphSize        :: V2 Int
  , glyphHoriBearing :: V2 Int
  , glyphVertBearing :: V2 Int
  , glyphAdvance     :: V2 Int
  } deriving (Show, Eq)
