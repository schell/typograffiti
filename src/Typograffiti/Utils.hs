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
-- , hasKerning
 , loadChar
 , loadGlyph
 , renderGlyph
 , newFace
 , setCharSize
 , setPixelSizes
 , withFreeType
 , runFreeType
 , ft_KERNING_DEFAULT, ft_KERNING_UNFITTED, ft_KERNING_UNSCALED
 , ft_LOAD_DEFAULT, ft_LOAD_NO_SCALE, ft_LOAD_RENDER, ft_LOAD_NO_BITMAP, ft_LOAD_VERTICAL_LAYOUT
 , ft_LOAD_FORCE_AUTOHINT, ft_LOAD_CROP_BITMAP, ft_LOAD_PEDANTIC, ft_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH
 , ft_LOAD_NO_RECURSE, ft_LOAD_IGNORE_TRANSFORM, ft_LOAD_MONOCHROME, ft_LOAD_LINEAR_DESIGN
 , ft_LOAD_NO_AUTOHINT, ft_LOAD_COLOR, ft_LOAD_COMPUTE_METRICS, ft_LOAD_BITMAP_METRICS_ONLY
) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Control.Monad (unless)
import           FreeType.Core.Base                                     as FT
import           FreeType.Core.Base.Internal                            as FT
import           FreeType.Core.Types                                    as FT
import           Foreign                                                as FT
import           Foreign.C.String                                       as FT
import           Unsafe.Coerce

-- TODO: Tease out the correct way to handle errors.
-- They're kinda thrown all willy nilly.

type FreeTypeT m = ExceptT String (StateT FT_Library m)
type FreeTypeIO = FreeTypeT IO


glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString FT_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
glyphFormatString FT_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
glyphFormatString FT_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
glyphFormatString FT_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
glyphFormatString _ = "ft_GLYPH_FORMAT_NONE"


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
    e <- ft_Init_FreeType' p
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
    alloca $ \ptr -> ft_New_Face' ft (unsafeCoerce str) 0 ptr >>= \case
      0 -> Right <$> peek ptr
      e -> return $ Left e

setCharSize :: (MonadIO m, Integral i) => FT_Face -> i -> i -> i -> i -> FreeTypeT m ()
setCharSize ff w h dpix dpiy = runIOErr "ft_Set_Char_Size" $
  ft_Set_Char_Size' ff (fromIntegral w)    (fromIntegral h)
                      (fromIntegral dpix) (fromIntegral dpiy)

setPixelSizes :: (MonadIO m, Integral i) => FT_Face -> i -> i -> FreeTypeT m ()
setPixelSizes ff w h =
  runIOErr "ft_Set_Pixel_Sizes" $ ft_Set_Pixel_Sizes' ff (fromIntegral w) (fromIntegral h)

getCharIndex :: (MonadIO m, Integral i)
             => FT_Face -> i -> FreeTypeT m FT_UInt
getCharIndex ff ndx = liftIO $ ft_Get_Char_Index ff $ fromIntegral ndx

loadGlyph :: MonadIO m => FT_Face -> FT_UInt -> FT_Int32 -> FreeTypeT m ()
loadGlyph ff fg flags = runIOErr "ft_Load_Glyph" $ ft_Load_Glyph' ff fg flags

loadChar :: MonadIO m => FT_Face -> FT_ULong -> FT_Int32 -> FreeTypeT m ()
loadChar ff char flags = runIOErr "ft_Load_Char" $ ft_Load_Char' ff char flags

renderGlyph :: MonadIO m => FT_GlyphSlot -> FreeTypeT m ()
renderGlyph glyph = runIOErr "ft_Render_Glyph" $ ft_Render_Glyph' glyph 0

--hasKerning :: MonadIO m => FT_Face -> FreeTypeT m Bool
--hasKerning = liftIO . ft_HAS_KERNING
--ft_HAS_KERNING FT_HAS_KERNING = return True
--ft_HAS_KERNING _ = return False

-- Matching patterns defined in freetype2 module.
ft_KERNING_DEFAULT, ft_KERNING_UNFITTED, ft_KERNING_UNSCALED :: Word32
ft_KERNING_DEFAULT = 0
ft_KERNING_UNFITTED = 1
ft_KERNING_UNSCALED = 2

ft_LOAD_DEFAULT, ft_LOAD_NO_SCALE, ft_LOAD_NO_HINTING, ft_LOAD_RENDER,
  ft_LOAD_NO_BITMAP, ft_LOAD_VERTICAL_LAYOUT, ft_LOAD_FORCE_AUTOHINT,
  ft_LOAD_CROP_BITMAP, ft_LOAD_PEDANTIC, ft_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH,
  ft_LOAD_NO_RECURSE, ft_LOAD_IGNORE_TRANSFORM, ft_LOAD_MONOCHROME,
  ft_LOAD_LINEAR_DESIGN, ft_LOAD_NO_AUTOHINT, ft_LOAD_COLOR,
  ft_LOAD_COMPUTE_METRICS, ft_LOAD_BITMAP_METRICS_ONLY :: FT_Int32
ft_LOAD_DEFAULT                     = 0
ft_LOAD_NO_SCALE                    = 1
ft_LOAD_NO_HINTING                  = 2
ft_LOAD_RENDER                      = 4
ft_LOAD_NO_BITMAP                   = 8
ft_LOAD_VERTICAL_LAYOUT             = 16
ft_LOAD_FORCE_AUTOHINT              = 32
ft_LOAD_CROP_BITMAP                 = 64
ft_LOAD_PEDANTIC                    = 128
ft_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH = 512
ft_LOAD_NO_RECURSE                  = 1024
ft_LOAD_IGNORE_TRANSFORM            = 2048
ft_LOAD_MONOCHROME                  = 4096
ft_LOAD_LINEAR_DESIGN               = 8192
ft_LOAD_NO_AUTOHINT                 = 32768
ft_LOAD_COLOR                       = 1048576
ft_LOAD_COMPUTE_METRICS             = 2097152
ft_LOAD_BITMAP_METRICS_ONLY         = 4194304

getKerning :: MonadIO m => FT_Face -> FT_UInt -> FT_UInt -> FT_Kerning_Mode -> FreeTypeT m (Int,Int)
getKerning ff prevNdx curNdx flags = liftE "ft_Get_Kerning" $ alloca $ \ptr ->
  ft_Get_Kerning' ff prevNdx curNdx (fromIntegral flags) ptr >>= \case
    0 -> do FT_Vector vx vy <- peek ptr
            return $ Right (fromIntegral vx, fromIntegral vy)
    e -> return $ Left e

getAdvance :: MonadIO m => FT_GlyphSlot -> FreeTypeT m (Int,Int)
getAdvance slot = do
  slot' <- liftIO $ peek slot
  let FT_Vector vx vy = gsrAdvance slot'
  return (fromIntegral vx, fromIntegral vy)
