{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module:     Typograffiti.Atlas
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides a font-character atlas to use in font rendering with
-- opengl.
--
module Typograffiti.Atlas where

import           Control.Monad
import           Control.Monad.Except                              (MonadError (..))
import           Control.Monad.IO.Class
import           Data.IntMap                                       (IntMap)
import qualified Data.IntMap                                       as IM
import           Data.Vector.Unboxed                               (Vector)
import qualified Data.Vector.Unboxed                               as UV
import           Foreign.Marshal.Utils                             (with)
import           Graphics.GL.Core32
import           Graphics.GL.Types
import           Graphics.Rendering.FreeType.Internal.Bitmap       as BM
import           Graphics.Rendering.FreeType.Internal.GlyphMetrics as GM
import           Linear

import           Typograffiti.GL
import           Typograffiti.Glyph
import           Typograffiti.Utils



data TypograffitiError =
    TypograffitiErrorNoGlyphMetricsForChar Char
  -- ^ The are no glyph metrics for this character. This probably means
  -- the character has not been loaded into the atlas.
  | TypograffitiErrorFreetype String String
  -- ^ There was a problem while interacting with the freetype2 library.
  | TypograffitiErrorGL String
  -- ^ There was a problem while interacting with OpenGL.
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- Atlas
--------------------------------------------------------------------------------


data Atlas = Atlas { atlasTexture     :: GLuint
                   , atlasTextureSize :: V2 Int
                   , atlasLibrary     :: FT_Library
                   , atlasFontFace    :: FT_Face
                   , atlasMetrics     :: IntMap GlyphMetrics
                   , atlasGlyphSize   :: GlyphSize
                   , atlasFilePath    :: FilePath
                   }


emptyAtlas :: FT_Library -> FT_Face -> GLuint -> Atlas
emptyAtlas lib fce t = Atlas t 0 lib fce mempty (GlyphSizeInPixels 0 0) ""


data AtlasMeasure = AM { amWH      :: V2 Int
                       , amXY      :: V2 Int
                       , rowHeight :: Int
                       } deriving (Show, Eq)


emptyAM :: AtlasMeasure
emptyAM = AM 0 (V2 1 1) 0


-- | The amount of spacing between glyphs rendered into the atlas's texture.
spacing :: Int
spacing = 1


-- | Extract the measurements of a character in the FT_Face and append it to
-- the given AtlasMeasure.
measure
  :: FT_Face
  -> Int
  -> (IntMap AtlasMeasure, AtlasMeasure)
  -> Char
  -> FreeTypeIO (IntMap AtlasMeasure, AtlasMeasure)
measure fce maxw (prev, am@AM{..}) char
  -- Skip chars that have already been measured
  | fromEnum char `IM.member` prev = return (prev, am)
  | otherwise = do
    let V2 x y = amXY
        V2 w h = amWH
    -- Load the char, replacing the glyph according to
    -- https://www.freetype.org/freetype2/docs/tutorial/step1.html
    loadChar fce (fromIntegral $ fromEnum char) ft_LOAD_RENDER
    -- Get the glyph slot
    slot <- liftIO $ peek $ glyph fce
    -- Get the bitmap
    bmp <- liftIO $ peek $ bitmap slot
    let bw = fromIntegral $ BM.width bmp
        bh = fromIntegral $ rows bmp
        gotoNextRow = (x + bw + spacing) >= maxw
        rh = if gotoNextRow then 0 else max bh rowHeight
        nx = if gotoNextRow then 0 else x + bw + spacing
        nw = max w (x + bw + spacing)
        nh = max h (y + rh + spacing)
        ny = if gotoNextRow then nh else y
        am1 = AM { amWH = V2 nw nh
                 , amXY = V2 nx ny
                 , rowHeight = rh
                 }
    return (IM.insert (fromEnum char) am prev, am1)


texturize :: IntMap (V2 Int) -> Atlas -> Char -> FreeTypeIO Atlas
texturize xymap atlas@Atlas{..} char
  | Just pos@(V2 x y) <- IM.lookup (fromEnum char) xymap = do
    -- Load the char
    loadChar atlasFontFace (fromIntegral $ fromEnum char) ft_LOAD_RENDER
    -- Get the slot and bitmap
    slot  <- liftIO $ peek $ glyph atlasFontFace
    bmp   <- liftIO $ peek $ bitmap slot
    -- Update our texture by adding the bitmap
    glTexSubImage2D
      GL_TEXTURE_2D
      0
      (fromIntegral x)
      (fromIntegral y)
      (fromIntegral $ BM.width bmp)
      (fromIntegral $ rows bmp)
      GL_RED
      GL_UNSIGNED_BYTE
      (castPtr $ buffer bmp)
    -- Get the glyph metrics
    ftms  <- liftIO $ peek $ metrics slot
    -- Add the metrics to the atlas
    let vecwh = fromIntegral <$> V2 (BM.width bmp) (rows bmp)
        canon = floor @Double @Int . (* 0.015625) . fromIntegral
        vecsz = canon <$> V2 (GM.width ftms) (GM.height ftms)
        vecxb = canon <$> V2 (horiBearingX ftms) (horiBearingY ftms)
        vecyb = canon <$> V2 (vertBearingX ftms) (vertBearingY ftms)
        vecad = canon <$> V2 (horiAdvance ftms) (vertAdvance ftms)
        mtrcs = GlyphMetrics { glyphTexBB = (pos, pos + vecwh)
                             , glyphTexSize = vecwh
                             , glyphSize = vecsz
                             , glyphHoriBearing = vecxb
                             , glyphVertBearing = vecyb
                             , glyphAdvance = vecad
                             }
    return atlas{ atlasMetrics = IM.insert (fromEnum char) mtrcs atlasMetrics }

  | otherwise = do
    liftIO $ putStrLn "could not find xy"
    return atlas

-- | Allocate a new 'Atlas'.
-- When creating a new 'Atlas' you must pass all the characters that you
-- might need during the life of the 'Atlas'. Character texturization only
-- happens once.
allocAtlas
  :: ( MonadIO m
     , MonadError TypograffitiError m
     )
  => FilePath
  -- ^ Path to the font file to use for this Atlas.
  -> GlyphSize
  -- ^ Size of glyphs in this Atlas.
  -> String
  -- ^ The characters to include in this 'Atlas'.
  -> m Atlas
allocAtlas fontFilePath gs str = do
  e <- liftIO $ runFreeType $ do
    fce <- newFace fontFilePath
    case gs of
      GlyphSizeInPixels w h -> setPixelSizes fce w h
      GlyphSizeByChar (CharSize w h dpix dpiy) -> setCharSize fce w h dpix dpiy

    (amMap, am) <- foldM (measure fce 512) (mempty, emptyAM) str

    let V2 w h = amWH am
        xymap :: IntMap (V2 Int)
        xymap  = amXY <$> amMap

    t <- liftIO $ do
      t <- allocAndActivateTex GL_TEXTURE0
      glPixelStorei GL_UNPACK_ALIGNMENT 1
      withCString (replicate (w * h) $ toEnum 0) $
        glTexImage2D GL_TEXTURE_2D 0 GL_RED (fromIntegral w) (fromIntegral h)
                     0 GL_RED GL_UNSIGNED_BYTE . castPtr
      return t

    lib   <- getLibrary
    atlas <- foldM (texturize xymap) (emptyAtlas lib fce t) str

    glGenerateMipmap GL_TEXTURE_2D
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    glBindTexture GL_TEXTURE_2D 0
    glPixelStorei GL_UNPACK_ALIGNMENT 4
    return
      atlas{ atlasTextureSize = V2 w h
           , atlasGlyphSize = gs
           , atlasFilePath = fontFilePath
           }

  either
    (throwError . TypograffitiErrorFreetype "cannot alloc atlas")
    (return . fst)
    e


-- | Releases all resources associated with the given 'Atlas'.
freeAtlas :: MonadIO m => Atlas -> m ()
freeAtlas a = liftIO $ do
  _ <- ft_Done_FreeType (atlasLibrary a)
  -- _ <- unloadMissingWords a ""
  with (atlasTexture a) $ \ptr -> glDeleteTextures 1 ptr


-- | Construct the geometry needed to render the given character.
makeCharQuad
  :: ( MonadIO m
     , MonadError TypograffitiError m
     )
  => Atlas
  -- ^ The atlas that contains the metrics for the given character.
  -> Bool
  -- ^ Whether or not to use kerning.
  -> Int
  -- ^ The current "pen position".
  -> Maybe FT_UInt
  -- ^ The freetype index of the previous character, if available.
  -> Char
  -- ^ The character to generate geometry for.
  -> m (Vector (V2 Float, V2 Float), Int, Maybe FT_UInt)
  -- ^ Returns the generated geometry (position in 2-space and UV parameters),
  -- the next pen position and the freetype index of the given character, if
  -- available.
makeCharQuad Atlas{..} useKerning penx mLast char = do
  let ichar = fromEnum char
  eNdx <- withFreeType (Just atlasLibrary) $ getCharIndex atlasFontFace ichar
  let mndx = either (const Nothing) Just eNdx
  px <- case (,,) <$> mndx <*> mLast <*> Just useKerning of
    Just (ndx,lndx,True) -> do
      e <- withFreeType (Just atlasLibrary) $
        getKerning atlasFontFace lndx ndx ft_KERNING_DEFAULT
      return $ either (const penx) ((+penx) . floor . (* 0.015625) . fromIntegral . fst) e
    _  -> return $ fromIntegral penx
  case IM.lookup ichar atlasMetrics of
    Nothing -> throwError $ TypograffitiErrorNoGlyphMetricsForChar char
    Just GlyphMetrics{..} -> do
      let V2 dx dy = fromIntegral <$> glyphHoriBearing
          x = fromIntegral px + dx
          y = -dy
          V2 w h = fromIntegral <$> glyphSize
          V2 aszW aszH = fromIntegral <$> atlasTextureSize
          V2 texL texT = fromIntegral <$> fst glyphTexBB
          V2 texR texB = fromIntegral <$> snd glyphTexBB

          tl = (V2 x      y   , V2 (texL/aszW) (texT/aszH))
          tr = (V2 (x+w)  y   , V2 (texR/aszW) (texT/aszH))
          br = (V2 (x+w) (y+h), V2 (texR/aszW) (texB/aszH))
          bl = (V2 x     (y+h), V2 (texL/aszW) (texB/aszH))
      let vs = UV.fromList [ tl, tr, br
                           , tl, br, bl
                           ]
      let V2 ax _ = glyphAdvance
      return (vs, px + ax, mndx)


-- | A string containing all standard ASCII characters.
-- This is often passed as the 'String' parameter in 'allocAtlas'.
asciiChars :: String
asciiChars = map toEnum [32..126]


-- | Generate the geometry of the given string.
stringTris
  :: ( MonadIO m
     , MonadError TypograffitiError m
     )
  => Atlas
  -- ^ The font atlas.
  -> Bool
  -- ^ Whether or not to use kerning.
  -> String
  -- ^ The string.
  -> m (Vector (V2 Float, V2 Float))
stringTris atlas useKerning str = do
  (vs, _, _) <- foldM gen (mempty, 0, Nothing) str
  return $ UV.concat vs
  where gen (vs, penx, mndx) c = do
          (newVs, newPenx, newMndx) <- makeCharQuad atlas useKerning penx mndx c
          return (vs ++ [newVs], newPenx, newMndx)
