{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | A storage context and operations for rendering text with multiple fonts
-- and sizes.
module Typograffiti.SDL.Store where


import           Control.Concurrent.STM     (atomically, newTMVar)
import           Control.Monad.Except       (MonadError (..))
import           Control.Monad.IO.Class     (MonadIO (..))
import qualified Data.Set                   as S
import           Linear
import           SDL                        (Texture, Renderer)
import qualified SDL

import           Typograffiti               (GlyphRenderingData (..), GlyphSize,
                                             RenderedGlyphs, Transform)
import qualified Typograffiti               as Core
import           Typograffiti.Freetype      (FT_Face, FT_Library)
import           Typograffiti.SDL.Atlas     (allocAtlas)
import           Typograffiti.SDL.Cache     (makeDefaultAllocateWord)
import           Typograffiti.SDL.Transform (Multiply, TextTransform, translate)
import           Typograffiti.Store         (Dictionary, Store (..))


-- | A pre-rendered bit of text, ready to display given
-- some post compilition transformations. Also contains
-- the text size.
type RenderedText = RenderedGlyphs [TextTransform]


-- | A cache of words and rasterised glyphs
type Font = Dictionary Texture (FT_Library, FT_Face) Char [Transform Multiply]


-- | All the data needed to render TTF font text quickly.
type TextRenderingData
  = GlyphRenderingData
      Texture
      (FT_Library, FT_Face)
      [TextTransform]


-- | Stored fonts at specific sizes.
type FontStore =
  Store
    Texture
    (FT_Library, FT_Face)
    [TextTransform]
    Char


getTextRendering
  :: ( MonadIO m
     , MonadError String m
     )
  => Renderer
  -- ^ The SDL 2d renderer.
  -> FontStore
  -- ^ The font store.
  -> FilePath
  -- ^ The path to the font to use
  -- for rendering.
  -> GlyphSize
  -- ^ The size of the font glyphs.
  -> String
  -- ^ The string to render.
  -> m (RenderedText m)
  -- ^ The rendered text, ready to draw to the screen.
getTextRendering r =
  Core.getRendering (allocAtlas r) translate Core.charGlyphAction


newFontStoreWithGlyphs
  :: ( MonadIO m
     , MonadError String m
     )
  => String
  -> Renderer
  -> m FontStore
newFontStoreWithGlyphs glyphs r = do
  aw <- makeDefaultAllocateWord r
  let dat =
        GlyphRenderingData
        { glyphRenderingDataAllocWord = aw
        , glyphRenderingDataDictMap   = mempty
        , glyphRenderingDataGlyphSet  = S.fromList glyphs
        }
  Store
    <$> liftIO (atomically $ newTMVar dat)


newDefaultFontStore
  :: ( MonadIO m
     , MonadError String m
     )
  => Renderer
  -> m FontStore
newDefaultFontStore =
  newFontStoreWithGlyphs Core.asciiChars
