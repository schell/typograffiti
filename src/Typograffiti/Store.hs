{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Module:     Typograffiti.Monad
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- A storage context an ops for rendering text with multiple fonts
-- and sizes, hiding the details of the Atlas and WordCache.
module Typograffiti.Store where


import           Control.Concurrent.STM (TMVar, atomically, newTMVar, putTMVar,
                                         readTMVar, takeTMVar)
import           Control.Monad.Except   (MonadError (..), liftEither)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Linear


import           Typograffiti.Atlas
import           Typograffiti.Cache
import           Typograffiti.Glyph

-- For font registration APIs
import           Typograffiti.Utils
import           FreeType.Support.Bitmap.Internal
import           FreeType.Support.Outline.Internal
import           FreeType.Support.Outline
import           FreeType.Core.Types
import           Data.Maybe             (fromMaybe)
import           System.IO


-- | A pre-rendered bit of text, ready to display given
-- some post compilition transformations. Also contains
-- the text size.
data RenderedText t m = RenderedText
  { drawRenderedText   :: t -> m ()
  , sizeOfRenderedText :: V2 Int
  }


data Font t = Font
  { fontAtlas     :: Atlas
  , fontWordCache :: WordCache t
  }


data TextRenderingData t = TextRenderingData
  { textRenderingDataAllocWord :: Atlas -> String -> IO (Either TypograffitiError (AllocatedRendering t))
  -- ^ The operation used to alloc a word.
  -- Generate geometry, use a shader program, set uniforms, etc.
  , textRenderingDataFontMap   :: Map (FilePath, GlyphSize) (Font t)
  -- ^ The cached fonts.
  , textRenderingDataCharSet   :: Set Char
  -- ^ The character set to have available in all allocated Atlas types.
  }


-- | Stored fonts at specific sizes.
newtype FontStore t = FontStore
  { unFontStore :: TMVar (TextRenderingData t)}


getTextRendering
  :: ( MonadIO m
     , MonadError TypograffitiError m
     , Layout t
     )
  => FontStore t
  -- ^ The font store.
  -> FilePath
  -- ^ The path to the font to use
  -- for rendering.
  -- Or alternatively: the `key`
  -- identifying a registered font.
  -> GlyphSize
  -- ^ The size of the font glyphs.
  -> String
  -- ^ The string to render.
  -> m (RenderedText t m)
  -- ^ The rendered text, ready to draw to the screen.
getTextRendering store file sz str = do
  let mvar = unFontStore store
  s    <- liftIO $ atomically $ readTMVar mvar
  font <- case M.lookup (file, sz) $ textRenderingDataFontMap s of
    Nothing   -> allocFont store file sz
    Just font -> return font
  (draw, tsz, cache) <-
    loadText
      (\x y -> liftIO (textRenderingDataAllocWord s x y) >>= liftEither)
      (fontAtlas font)
      (fontWordCache font)
      str
  liftIO
    $ atomically $ do
      s1 <- takeTMVar mvar
      let alterf Nothing               = Just $ Font (fontAtlas font) cache
          alterf (Just (Font atlas _)) = Just $ Font atlas cache
          fontmap = M.alter alterf (file,sz)
            $ textRenderingDataFontMap s1
      putTMVar mvar s1{ textRenderingDataFontMap = fontmap }
  return RenderedText
    { drawRenderedText   = liftIO . draw
    , sizeOfRenderedText = tsz
    }


newDefaultFontStore
  :: ( MonadIO m
     , MonadError TypograffitiError m
     , Integral i
     )
  => IO (V2 i)
  -> m (FontStore [TextTransform])
newDefaultFontStore getDims = do
  aw <- makeDefaultAllocateWord getDims
  let dat = TextRenderingData
        { textRenderingDataAllocWord = aw
        , textRenderingDataFontMap   = mempty
        , textRenderingDataCharSet   = S.fromList asciiChars
        }
  FontStore
    <$> liftIO (atomically $ newTMVar dat)

newFontStoreForCharset
  :: ( MonadIO m
     , MonadError TypograffitiError m
     , Integral i
     )
  => IO (V2 i)
  -> String
  -> Bool -- ^ If True silences errors, rendering Tofu glyphs instead.
  -> m (FontStore [TextTransform])
newFontStoreForCharset getDims charset useTofu = do
  aw <- makeDefaultAllocateWord getDims
  let addTofu = if useTofu then ('\0':) else id
  let dat = TextRenderingData
        { textRenderingDataAllocWord = aw
        , textRenderingDataFontMap   = mempty
        , textRenderingDataCharSet   = S.fromList $ addTofu charset
        }
  FontStore
    <$> liftIO (atomically $ newTMVar dat)


allocFont
  :: ( MonadIO m
     , MonadError TypograffitiError m
     , Layout t
     )
  => FontStore t
  -> FilePath
  -> GlyphSize
  -> m (Font t)
allocFont store file sz = do
  let mvar = unFontStore store
  s     <- liftIO $ atomically $ takeTMVar mvar
  atlas <-
    allocAtlas
      file
      sz
      $ S.toList
      $ textRenderingDataCharSet s
  let fontmap = textRenderingDataFontMap s
      font = Font
        { fontAtlas     = atlas
        , fontWordCache = mempty
        }
  liftIO
    $ atomically
    $ putTMVar mvar
    $ s{ textRenderingDataFontMap = M.insert (file, sz) font fontmap }
  return font

registerFont
  :: Layout t
  => FontStore t
  -> String
  -> FT_Face
  -> Maybe GlyphSize
  -> (FT_GlyphSlot -> FreeTypeIO ())
  -> FreeTypeIO (Font t)
-- | Register an externally-loaded font under a given key (low-level API)
-- Allows registering a callback for mutating glyphs prior
-- to being composited into place on the GPU, which is
-- responsible for ensuring Typograffiti has a bitmap to composite.
registerFont store key fce sz cb = do
  s <- liftIO $ atomically $ readTMVar $ unFontStore store
  registerFontWithCharset store key fce sz cb (textRenderingDataCharSet s) False

registerFontWithCharset
  :: Layout t
  => FontStore t
  -> String
  -> FT_Face
  -> Maybe GlyphSize
  -> (FT_GlyphSlot -> FreeTypeIO ())
  -> Set Char
  -> Bool -- ^ If True silences errors, rendering Tofu glyphs instead.
  -> FreeTypeIO (Font t)
registerFontWithCharset store key fce sz cb charset useTofu = do
  let mvar = unFontStore store
  s     <- liftIO $ atomically $ takeTMVar mvar
  atlas <-
    allocRichAtlas
      key
      fce
      sz
      cb
      $ (if useTofu then ('\0':) else id)
      $ S.toList charset
  let fontmap = textRenderingDataFontMap s
      font = Font
        { fontAtlas     = atlas
        , fontWordCache = mempty
        }
  let sz' = atlasGlyphSize atlas
  liftIO
    $ atomically
    $ putTMVar mvar
    $ s{ textRenderingDataFontMap = M.insert (key, sz') font fontmap }
  return font

registerStyledFont
  :: ( MonadIO m
     , MonadError TypograffitiError m
     , Layout t
     )
  => FontStore t
  -> String
  -- ^ Key by which to identify this styled font
  -> FilePath
  -- ^ Path to the raw fontfile
  -> FT_Pos
  -- ^ How much to embolden the font
  -- Negative values lighten the font.
  -> Maybe FT_Pos
  -- ^ How much to embolden the font vertically, if different from horizontally.
  -> FT_Fixed
  -- ^ How much to slant the font, approximating italics.
  -> GlyphSize
  -- ^ The desired fontsize
  -> m (Font t)
-- | Registers font under the given key modified to approximate the desired boldness & obliqueness.
-- Adds negligable CPU latency,
-- but best results always come from giving the font designing full artistic control.
-- Obliqueness isn't currently supported on bitmap fonts.
registerStyledFont store key file weight vweight slant sz = do
    e <- liftIO $ runFreeType $ do
      lib <- getLibrary
      fce <- newFace file
      registerFont store key fce (Just sz) $ modifyGlyph lib

    either
      (throwError . TypograffitiErrorFreetype "cannot alloc atlas")
      (return . fst)
      e
  where
    modifyGlyph lib glyf = do
      glyf' <- liftIO $ peek glyf
      case gsrFormat glyf' of
        FT_GLYPH_FORMAT_OUTLINE -> modifyOutline glyf
        FT_GLYPH_FORMAT_BITMAP -> modifyBitmap lib glyf
        x -> liftIO $ do
          hPrint stderr "Unsupported glyph format:"
          hPrint stderr x
    modifyOutline glyf = do
      let outline = gsrOutline' glyf
      runIOErr "ft_Outline_EmboldenXY" $
          ft_Outline_EmboldenXY' outline weight $ fromMaybe weight vweight
      liftIO $ ft_Outline_Transform outline $ FT_Matrix 1 slant 0 1
      renderGlyph glyf
    modifyBitmap lib glyf = do
      let bitmap = gsrBitmap' glyf
      runIOErr "ft_Bitmap_Embolden" $
          ft_Bitmap_Embolden' lib bitmap weight $ fromMaybe weight vweight
      -- FreeType doesn't have a transform method on bitmaps.
