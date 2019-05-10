{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Typograffiti.Freetype
 ( -- * Helpers for building freetype backends
   measure
 , atlasLibrary
 , atlasFontFace
 , emptyAtlas
 , makeCharQuad
 , asciiChars
 , stringTris
 , stringQuads
 , quadsBounds
 , boundingBox
   -- * Freetype stuff...
 , module FT
 , FreeTypeT
 , FreeTypeIO
 , FT_Glyph_Metrics
 , getFreetypeChar
 , getAdvance
 , getCharIndex
 , getLibrary
 , getKerning
 , glyphFormatString
 , hasKerning
 , loadChar
 , loadGlyph
 , newFace
 , setCharSize
 , setPixelSizes
 , withFreeType
 , runFreeType
) where

import           Control.Monad                                       (unless)
import           Control.Monad.Except
import           Control.Monad.IO.Class                              (MonadIO,
                                                                      liftIO)
import           Control.Monad.State.Strict
import           Data.IntMap.Strict                                  (IntMap)
import qualified Data.IntMap.Strict                                  as IM
import           Data.Vector.Unboxed                                 (Unbox)
import qualified Data.Vector.Unboxed                                 as UV
import           Foreign                                             as FT hiding
                                                                            (void)
import           Foreign.C.String                                    as FT
import           Graphics.Rendering.FreeType.Internal                as FT
import           Graphics.Rendering.FreeType.Internal.Bitmap         as FT
import           Graphics.Rendering.FreeType.Internal.Face           as FT hiding
                                                                            (generic)
import           Graphics.Rendering.FreeType.Internal.GlyphMetrics   (FT_Glyph_Metrics)
import           Graphics.Rendering.FreeType.Internal.GlyphSlot      as FT
import           Graphics.Rendering.FreeType.Internal.Library        as FT
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT
import           Graphics.Rendering.FreeType.Internal.Vector         as FT
import           Linear                                              (V2 (..))

import           Typograffiti.Atlas                                  (Atlas (..),
                                                                      AtlasMeasure (..))
import           Typograffiti.Glyph                                  (GlyphMetrics (..),
                                                                      GlyphSize (..))


type FreeTypeT m = ExceptT FT_Error (StateT FT_Library m)
type FreeTypeIO = FreeTypeT IO


glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"


liftE :: MonadIO m => String -> IO (Either FT_Error a) -> FreeTypeT m a
liftE msg f = liftIO f >>= \case
  Left e  -> fail $ unwords [msg, show e]
  Right a -> return a


runIOErr :: MonadIO m => String -> IO FT_Error -> FreeTypeT m ()
runIOErr msg f = do
  e <- liftIO f
  unless (e == 0) $ fail $ unwords [msg, show e]


runFreeType :: MonadIO m => FreeTypeT m a -> m (Either FT_Error (a, FT_Library))
runFreeType f = do
  (e,lib) <- liftIO $ alloca $ \p -> do
    e <- ft_Init_FreeType p
    lib <- peek p
    return (e,lib)
  if e /= 0
    then do
      _ <- liftIO $ ft_Done_FreeType lib
      return $ Left e
    else fmap (,lib) <$> evalStateT (runExceptT f) lib

withFreeType :: MonadIO m => Maybe FT_Library -> FreeTypeT m a -> m (Either FT_Error a)
withFreeType Nothing f = runFreeType f >>= \case
  Left e -> return $ Left e
  Right (a,lib) -> do
    _ <- liftIO $ ft_Done_FreeType lib
    return $ Right a
withFreeType (Just lib) f = evalStateT (runExceptT f) lib

getLibrary :: MonadIO m => FreeTypeT m FT_Library
getLibrary = lift get

newFace :: MonadIO m => FilePath -> FreeTypeT m FT_Face
newFace fp = do
  ft <- lift get
  liftE "ft_New_Face" $ withCString fp $ \str ->
    alloca $ \ptr -> ft_New_Face ft str 0 ptr >>= \case
      0 -> Right <$> peek ptr
      e -> return $ Left e

setCharSize :: (MonadIO m, Integral i) => FT_Face -> i -> i -> i -> i -> FreeTypeT m ()
setCharSize ff w h dpix dpiy = runIOErr "ft_Set_Char_Size" $
  ft_Set_Char_Size ff (fromIntegral w)    (fromIntegral h)
                      (fromIntegral dpix) (fromIntegral dpiy)

setPixelSizes :: (MonadIO m, Integral i) => FT_Face -> i -> i -> FreeTypeT m ()
setPixelSizes ff w h =
  runIOErr "ft_Set_Pixel_Sizes" $ ft_Set_Pixel_Sizes ff (fromIntegral w) (fromIntegral h)

getCharIndex :: (MonadIO m, Integral i)
             => FT_Face -> i -> FreeTypeT m FT_UInt
getCharIndex ff ndx = liftIO $ ft_Get_Char_Index ff $ fromIntegral ndx

loadGlyph :: MonadIO m => FT_Face -> FT_UInt -> FT_Int32 -> FreeTypeT m ()
loadGlyph ff fg flags = runIOErr "ft_Load_Glyph" $ ft_Load_Glyph ff fg flags

loadChar :: MonadIO m => FT_Face -> FT_ULong -> FT_Int32 -> FreeTypeT m ()
loadChar ff char flags = runIOErr "ft_Load_Char" $ ft_Load_Char ff char flags

hasKerning :: MonadIO m => FT_Face -> FreeTypeT m Bool
hasKerning = liftIO . ft_HAS_KERNING

getKerning :: MonadIO m => FT_Face -> FT_UInt -> FT_UInt -> FT_Kerning_Mode -> FreeTypeT m (Int,Int)
getKerning ff prevNdx curNdx flags = liftE "ft_Get_Kerning" $ alloca $ \ptr ->
  ft_Get_Kerning ff prevNdx curNdx (fromIntegral flags) ptr >>= \case
    0 -> do FT_Vector vx vy <- peek ptr
            return $ Right (fromIntegral vx, fromIntegral vy)
    e -> return $ Left e

getAdvance :: MonadIO m => FT_GlyphSlot -> FreeTypeT m (Int,Int)
getAdvance slot = do
  FT_Vector vx vy <- liftIO $ peek $ advance slot
  return (fromIntegral vx, fromIntegral vy)


--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------


atlasLibrary :: Atlas tex (FT_Library, FT_Face) -> FT_Library
atlasLibrary = fst . atlasResources


atlasFontFace :: Atlas tex (FT_Library, FT_Face)  -> FT_Face
atlasFontFace = snd . atlasResources


emptyAtlas :: FT_Library -> FT_Face -> tex -> Atlas tex (FT_Library, FT_Face)
emptyAtlas lib fce t = Atlas t (lib, fce) 0 mempty (GlyphSizeInPixels 0 0) ""


getFreetypeChar
  :: MonadIO m
  => Atlas tex (FT_Library, FT_Face)
  -> Char
  -> FreeTypeT m (FT_Bitmap, FT_Glyph_Metrics)
getFreetypeChar atlas char = do
  -- Load the char
  loadChar
    (atlasFontFace atlas)
    (fromIntegral $ fromEnum char)
    ft_LOAD_RENDER
  -- Get the slot and bitmap
  slot <-
    liftIO
      $ peek
      $ glyph
      $ atlasFontFace atlas
  (,)
    <$> liftIO (peek $ bitmap slot)
    <*> liftIO (peek $ metrics slot)



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
        -- The amount of spacing between glyphs rendered into the atlas's
        -- texture.
        spacing = 1
    -- Load the char, replacing the glyph according to
    -- https://www.freetype.org/freetype2/docs/tutorial/step1.html
    loadChar fce (fromIntegral $ fromEnum char) ft_LOAD_RENDER
    -- Get the glyph slot
    slot <- liftIO $ peek $ glyph fce
    -- Get the bitmap
    bmp <- liftIO $ peek $ bitmap slot
    let bw = fromIntegral $ FT.width bmp
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


type Tri a = (a, a, a)
type V2UVTri = Tri (V2 Float, V2 Float)
type TriQuad = (V2UVTri, V2UVTri)


-- | Construct the geometry needed to render the given character.
makeCharQuad
  :: ( MonadIO m
     , MonadError String m
     )
  => Atlas tex (FT_Library, FT_Face)
  -- ^ The atlas that contains the metrics for the given character.
  -> Bool
  -- ^ Whether or not to use kerning.
  -> Int
  -- ^ The current "pen position".
  -> Maybe FT_UInt
  -- ^ The freetype index of the previous character, if available.
  -> Char
  -- ^ The character to generate geometry for.
  -> m (TriQuad, Int, Maybe FT_UInt)
  -- ^ Returns the generated geometry (position in 2-space and UV parameters),
  -- the next pen position and the freetype index of the given character, if
  -- available.
makeCharQuad atlas useKerning penx mLast char = do
  let ichar = fromEnum char
  eNdx <-
    withFreeType
      (Just $ atlasLibrary atlas)
      $ getCharIndex (atlasFontFace atlas) ichar
  let mndx = either (const Nothing) Just eNdx
  px <- case (,,) <$> mndx <*> mLast <*> Just useKerning of
    Just (ndx,lndx,True) -> do
      e <- withFreeType (Just $ atlasLibrary atlas) $
        getKerning (atlasFontFace atlas) lndx ndx ft_KERNING_DEFAULT
      return
        $ either
            (const penx)
            ((+penx) . floor . (* (0.015625 :: Double)) . fromIntegral . fst)
            e
    _  -> return $ fromIntegral penx
  case IM.lookup ichar $ atlasMetrics atlas of
    Nothing -> throwError $ "No glyph metrics for glyph: " ++ show char
    Just GlyphMetrics{..} -> do
      let V2 dx dy = fromIntegral <$> glyphHoriBearing
          x = fromIntegral px + dx
          y = -dy
          V2 w h = fromIntegral <$> glyphSize
          V2 aszW aszH = fromIntegral <$> atlasTextureSize atlas
          V2 texL texT = fromIntegral <$> fst glyphTexBB
          V2 texR texB = fromIntegral <$> snd glyphTexBB

          tl = (V2 x      y   , V2 (texL/aszW) (texT/aszH))
          tr = (V2 (x+w)  y   , V2 (texR/aszW) (texT/aszH))
          br = (V2 (x+w) (y+h), V2 (texR/aszW) (texB/aszH))
          bl = (V2 x     (y+h), V2 (texL/aszW) (texB/aszH))
      let tri1 = (tl, tr, br)
          tri2 = (tl, br, bl)
      let V2 ax _ = glyphAdvance
      return ((tri1, tri2), px + ax, mndx)


-- | A string containing all standard ASCII characters.
-- This is often passed as the 'String' parameter in 'allocAtlas'.
asciiChars :: String
asciiChars = map toEnum [32..126]


stringGeom
  :: forall m b tex
   . ( MonadIO m
     , MonadError String m
     , Unbox b
     )
  => (TriQuad -> UV.Vector b)
  -- ^ The function used to expand a quad.
  -> Atlas tex (FT_Library, FT_Face)
  -- ^ The font atlas.
  -> Bool
  -- ^ Whether or not to use kerning.
  -> String
  -- ^ The string.
  -> m (UV.Vector b)
stringGeom f atlas useKerning str = do
  tqs <- UV.unfoldrM gen (0, Nothing, str)
  return $ UV.concatMap f tqs
  where
    gen
      :: (Int, Maybe FT_UInt, String)
      -> m (Maybe (TriQuad, (Int, Maybe FT_UInt, String)))
    gen (_, _, []) = return Nothing
    gen (penx, mndx, c:chars) = do
      (triquad, newPenx, newMndx) <- makeCharQuad atlas useKerning penx mndx c
      return $ Just (triquad, (newPenx, newMndx, chars))


-- | A bounding box represented by the top left and bottom right points.
type Quad = (V2 Float, V2 Float)


-- | Generate the geometry of the given string in (pos, uv) pairs, where each
-- three pairs make a triangle.
stringTris
  :: ( MonadIO m
     , MonadError String m
     )
  => Atlas tex (FT_Library, FT_Face)
  -- ^ The font atlas.
  -> Bool
  -- ^ Whether or not to use kerning.
  -> String
  -- ^ The string.
  -> m (UV.Vector (V2 Float, V2 Float))
stringTris =
  stringGeom
    $ \((tl, tr, br), (_tl, _br, bl)) ->
        UV.fromList [ tl, tr, br, _tl, _br, bl ]


-- | Generate the geometry of the given string in (quad, quad) pairs, where each
-- quad is (top left, bottom right) points of the quad.
stringQuads
  :: ( MonadIO m
     , MonadError String m
     )
  => Atlas tex (FT_Library, FT_Face)
  -- ^ The font atlas.
  -> Bool
  -- ^ Whether or not to use kerning.
  -> String
  -- ^ The string.
  -> m (UV.Vector (Quad, Quad))
  -- ^ A vector of quad pairs, where each pair is (destination quad, source quad)
stringQuads =
  stringGeom
    $ \((tl, _tr, br), _) ->
        UV.singleton ((fst tl, fst br), (snd tl, snd br))


-- | TODO: calculate the size in `stringGeom`.
quadsBounds
  :: UV.Vector (Quad, Quad)
  -> (V2 Float, V2 Float)
quadsBounds =
  boundingBox
  . UV.concatMap (\(tl, br) -> UV.fromList [tl, br])
  . fst
  . UV.unzip


boundingBox
  :: ( Unbox a
     , Real a
     , Fractional a
     )
  => UV.Vector (V2 a)
  -> (V2 a, V2 a)
boundingBox vs
  | UV.null vs = (0,0)
  | otherwise = UV.foldl' f (br,tl) vs
  where mn a = min a . realToFrac
        mx a = max a . realToFrac
        f (a, b) c = (mn <$> a <*> c, mx <$> b <*> c)
        inf = 1/0
        ninf = (-1)/0
        tl = V2 ninf ninf
        br = V2 inf inf
