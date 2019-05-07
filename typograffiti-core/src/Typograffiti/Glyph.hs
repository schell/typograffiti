module Typograffiti.Glyph where


import           Linear (V2)


-- | The size of one freetype font character.
-- https://www.freetype.org/freetype2/docs/tutorial/step1.html#section-5
data CharSize = CharSize
  { charSizeWidth     :: Int
    -- ^ Width of a character specified in 1/64 of points.
  , charSizeHeight    :: Int
    -- ^ Height of a character specified in 1/64 of points.
  , charSizeWidthDPI  :: Int
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


-- | Knowledge about a file's set of glyphs.
-- https://www.freetype.org/freetype2/docs/tutorial/step2.html
data GlyphMetrics
  = GlyphMetrics
  { glyphTexBB       :: (V2 Int, V2 Int)
  -- ^ The bounding box around the glyph
  , glyphTexSize     :: V2 Int
  -- ^ The total texture size
  , glyphSize        :: V2 Int
  -- ^ One glyph's size
  , glyphHoriBearing :: V2 Int
  , glyphVertBearing :: V2 Int
  , glyphAdvance     :: V2 Int
  } deriving (Show, Eq)


-- | An action that should be taken by the renderer depending on the glyph.
-- This is used to represent rendering spaces and newlines in strings, since
-- those glyphs (' ' and '\n') don't have any real rendering steps and instead
-- cause the renderer to jump through 2d space.
data GlyphAction
  = GlyphActionNewline
  -- ^ This glyph action causes the renderer to return to its starting x and
  -- drop down y by the glyph height.
  | GlyphActionSpace
  -- ^ This glyph action causes the renderer to jump right in x by the glyph
  -- width without rendering.
  | GlyphActionRender
  -- ^ This glyph action causes the renderer to render the glyph and jump right
  -- by the glyph's advance.
  deriving (Eq)


-- | Determine the GlyphAction based on Char.
charGlyphAction :: Char -> GlyphAction
charGlyphAction '\n' = GlyphActionNewline
charGlyphAction ' '  = GlyphActionSpace
charGlyphAction _    = GlyphActionRender


isWhiteSpace :: (a -> GlyphAction) -> a -> Bool
isWhiteSpace = ((/= GlyphActionRender) .)


isRenderGlyph :: (a -> GlyphAction) -> a -> Bool
isRenderGlyph = ((== GlyphActionRender) .)


-- | Break a list of glyphs into whitespace separated lists.
--
-- >>> breakWords charGlyphAction "this is a string"
-- ["this","is","a","string"]
breakWords :: (a -> GlyphAction) -> [a] -> [[a]]
breakWords mkAction =
  uncurry (:)
  . foldr accum ([], [])
  where
    accum c (w, ws) =
      if isWhiteSpace mkAction c
      then ([], w:ws)
      else (c:w, ws)
