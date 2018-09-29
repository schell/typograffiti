{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Typograffiti.Utils (
   module FT
 , FreeTypeT
 , FreeTypeIO
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

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad (unless)
import           Graphics.Rendering.FreeType.Internal                   as FT
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes    as FT
import           Graphics.Rendering.FreeType.Internal.Library           as FT
import           Graphics.Rendering.FreeType.Internal.FaceType          as FT
import           Graphics.Rendering.FreeType.Internal.Face as FT hiding (generic)
import           Graphics.Rendering.FreeType.Internal.GlyphSlot         as FT
import           Graphics.Rendering.FreeType.Internal.Bitmap            as FT
import           Graphics.Rendering.FreeType.Internal.Vector            as FT
import           Foreign                                                as FT
import           Foreign.C.String                                       as FT

-- TODO: Tease out the correct way to handle errors.
-- They're kinda thrown all willy nilly.

type FreeTypeT m = ExceptT String (StateT FT_Library m)
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


runFreeType :: MonadIO m => FreeTypeT m a -> m (Either String (a, FT_Library))
runFreeType f = do
  (e,lib) <- liftIO $ alloca $ \p -> do
    e <- ft_Init_FreeType p
    lib <- peek p
    return (e,lib)
  if e /= 0
    then do
      _ <- liftIO $ ft_Done_FreeType lib
      return $ Left $ "Error initializing FreeType2:" ++ show e
    else fmap (,lib) <$> evalStateT (runExceptT f) lib

withFreeType :: MonadIO m => Maybe FT_Library -> FreeTypeT m a -> m (Either String a)
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
  runIOErr "ft_Set_Pixel_Sizess" $ ft_Set_Pixel_Sizes ff (fromIntegral w) (fromIntegral h)

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
  liftIO $ print ("v", vx, vy)
  return (fromIntegral vx, fromIntegral vy)
