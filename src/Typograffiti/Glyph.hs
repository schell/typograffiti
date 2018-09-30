module Typograffiti.Glyph where


import Linear


-- | The size of one freetype font character.
-- https://www.freetype.org/freetype2/docs/tutorial/step1.html#section-5
data CharSize = CharSize
  { charSizeWidth  :: Int
    -- ^ Width of a character specified in 1/64 of points.
  , charSizeHeight :: Int
    -- ^ Height of a character specified in 1/64 of points.
  , charSizeWidthDPI :: Int
    -- ^ Horizontal device resolution
  , charSizeHeightDPI :: Int
    -- ^ Vertical device resolution
  } deriving (Show, Eq, Ord)


data GlyphSize = GlyphSizeByChar CharSize
               | GlyphSizeInPixels Int Int
               deriving (Show, Eq, Ord)


pixelWidth :: GlyphSize -> Float
pixelWidth (GlyphSizeInPixels w h)
  | w == 0 = fromIntegral h
  | otherwise = fromIntegral w
pixelWidth (GlyphSizeByChar (CharSize w h xdpi ydpi)) =
  let dpi = if xdpi == 0 then ydpi else xdpi
      sz  = if w == 0 then h else w
  in fromIntegral sz * fromIntegral dpi / 72


pixelHeight :: GlyphSize -> Float
pixelHeight (GlyphSizeInPixels w h)
  | h == 0 = fromIntegral w
  | otherwise = fromIntegral h
pixelHeight (GlyphSizeByChar (CharSize w h xdpi ydpi)) =
  let dpi = if ydpi == 0 then xdpi else ydpi
      sz  = if h == 0 then w else h
  in fromIntegral sz * fromIntegral dpi / 72


-- | https://www.freetype.org/freetype2/docs/tutorial/step2.html
data GlyphMetrics = GlyphMetrics
  { glyphTexBB       :: (V2 Int, V2 Int)
  , glyphTexSize     :: V2 Int
  , glyphSize        :: V2 Int
  , glyphHoriBearing :: V2 Int
  , glyphVertBearing :: V2 Int
  , glyphAdvance     :: V2 Int
  } deriving (Show, Eq)
