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
import           Typograffiti.Utils     (FT_Face, FT_GlyphSlot, FreeTypeIO(..))


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
  let mvar = unFontStore store
  s     <- liftIO $ atomically $ takeTMVar mvar
  atlas <-
    allocRichAtlas
      key
      fce
      sz
      cb
      $ S.toList
      $ textRenderingDataCharSet s
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
