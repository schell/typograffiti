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
  , asciiChars
  , stringTris
  ) where

import           Typograffiti.Atlas
import           Typograffiti.Glyph


--------------------------------------------------------------------------------
-- WordMap
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Picture
--------------------------------------------------------------------------------
-- | Constructs a 'TexturePictureT' of one word in all red.
-- V4ization can then be done using 'setReplacementV4' in the picture
-- computation, or by using 'redChannelReplacement' and passing that to the
-- renderer after compilation, at render time. Keep in mind that any new word
-- geometry will be discarded, since this computation does not return a new 'Atlas'.
-- For that reason it is advised that you load the needed words before using this
-- function. For loading words, see 'loadWords'.
--
-- This is used in 'freetypeFontRendering' to construct the geometry of each word.
-- 'freetypeFontRendering' goes further and stores these geometries, looking them up
-- and constructing a string of word renderers for each input 'String'.
--freetypePicture
--  :: MonadIO m
--  => Atlas
--  -- ^ The 'Atlas' from which to read font textures word geometry.
--  -> String
--  -- ^ The word to render.
--  -> m FontRendering
--  -- ^ Returns a textured picture computation representing the texture and
--  -- geometry of the input word.
--freetypePicture atlas@Atlas{..} str = do
--  eKerning <- withFreeType (Just atlasLibrary) $ hasKerning atlasFontFace
--  setTextures [atlasTexture]
--  let useKerning = either (const False) id eKerning
--  setGeometry $ triangles $ stringTris atlas useKerning str
--------------------------------------------------------------------------------
-- Performance FontRendering
--------------------------------------------------------------------------------
-- | Constructs a 'FontRendering' from the given color and string. The 'WordMap'
-- record of the given 'Atlas' is used to construct the string geometry, greatly
-- improving performance and allowing longer strings to be compiled and renderered
-- in real time. To create a new 'Atlas' see 'allocAtlas'.
--
-- Note that since word geometries are stored in the 'Atlas' 'WordMap' and multiple
-- renderers can reference the same 'Atlas', the returned 'FontRendering' contains a
-- clean up operation that does nothing. It is expected that the programmer
-- will call 'freeAtlas' manually when the 'Atlas' is no longer needed.
--freetypeFontRendering
--  :: MonadIO m
--  => SomeProgram
--  -- ^ The V2(backend, to) use for compilation.
--  -> Atlas
--  -- ^ The 'Atlas' to read character textures from and load word geometry
--  -- into.
--  -> V4 Float
--  -- ^ The solid color to render the string with.
--  -> String
--  -- ^ The string to render.
--  -- This string can contain newlines, which will be respected.
--  -> m (FontRendering, V2 Float, Atlas)
--  -- ^ Returns the 'FontRendering', the size of the text and the new
--  -- 'Atlas' with the loaded geometry of the string.
--freetypeFontRendering b atlas0 color str = do
--  atlas <- loadWords b atlas0 str
--  let glyphw  = glyphWidth $ atlasGlyphSize atlas
--      spacew  = fromMaybe glyphw $ do
--        metrcs <- IM.lookup (fromEnum ' ') $ atlasMetrics atlas
--        let (x, _) = glyphAdvance metrcs
--        return $ fromIntegral x
--      glyphh = glyphHeight $ atlasGlyphSize atlas
--      spaceh = glyphh
--      isWhiteSpace c = c == ' ' || c == '\n' || c == '\t'
--      renderWord :: [FontTransform] -> V2 Float -> String -> IO ()
--      renderWord _ _ ""       = return ()
--      renderWord rs (V2 _ y) ('\n':cs) = renderWord rs (V2 0 (y + spaceh)) cs
--      renderWord rs (V2 x y) (' ':cs) = renderWord rs (V2 (x + spacew) y) cs
--      renderWord rs (V2 x y) cs       = do
--        let word = takeWhile (not . isWhiteSpace) cs
--            rest = drop (length word) cs
--        case M.lookup word (atlasWordMap atlas) of
--          Nothing          -> renderWord rs (V2 x y) rest
--          Just (V2 w _, r) -> do
--            let ts = [move x y, redChannelReplacementV4 color]
--            snd r $ ts ++ rs
--            renderWord rs (V2 (x + w) y) rest
--      rr t = renderWord t 0 str
--      measureString :: (V2 Float, V2 Float) -> String -> (V2 Float, V2 Float)
--      measureString (V2 x y, V2 w h) ""        = (V2 x y, V2 w h)
--      measureString (V2 x y, V2 w _) (' ':cs)  =
--        let nx = x + spacew in measureString (V2 nx y, V2 (max w nx) y) cs
--      measureString (V2 x y, V2 w h) ('\n':cs) =
--        let ny = y + spaceh in measureString (V2 x ny, V2 w (max h ny)) cs
--      measureString (V2 x y, V2 w h) cs        =
--        let word = takeWhile (not . isWhiteSpace) cs
--            rest = drop (length word) cs
--            n    = case M.lookup word (atlasWordMap atlas) of
--                     Nothing          -> (V2 x y, V2 w h)
--                     Just (V2 ww _, _) -> let nx = x + ww
--                                          in (V2 nx y, V2 (max w nx) y)
--        in measureString n rest
--      (szw, szh) = snd $ measureString (0,0) str
--  return ((return (), rr), V2 szw (max spaceh szh), atlas)
