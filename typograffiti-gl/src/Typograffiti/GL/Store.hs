{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | A storage context and operations for rendering text with multiple fonts
-- and sizes.
module Typograffiti.GL.Store where


import           Control.Concurrent.STM       (atomically, newTMVar)
import           Control.Monad.Except         (MonadError (..))
import           Control.Monad.IO.Class       (MonadIO (..))
import qualified Data.Set                     as S
import           Linear


import           Typograffiti                 (GlyphRenderingData (..),
                                               GlyphSize, RenderedGlyphs,
                                               Transform)
import qualified Typograffiti                 as Core
import           Typograffiti.Store           (Dictionary, Store (..))

import           Typograffiti.Freetype        (FT_Face, FT_Library)
import           Typograffiti.GL.Atlas        (allocAtlas)
import           Typograffiti.GL.Cache        (makeDefaultAllocateWord)
import           Typograffiti.GL.Transform    (Multiply, TextTransform,
                                               translate)
import           Typograffiti.GL.Utils.OpenGL (GLuint)


-- | A pre-rendered bit of text, ready to display given
-- some post compilition transformations. Also contains
-- the text size.
type RenderedText = RenderedGlyphs [TextTransform]


-- | A cache of words and rasterised glyphs
type Font = Dictionary GLuint (FT_Library, FT_Face) Char [Transform Multiply]


-- | All the data needed to render TTF font text quickly.
type TextRenderingData
  = GlyphRenderingData
      GLuint
      (FT_Library, FT_Face)
      [TextTransform]


-- | Stored fonts at specific sizes.
type FontStore =
  Store
    GLuint
    (FT_Library, FT_Face)
    [TextTransform]
    Char


getTextRendering
  :: ( MonadIO m
     , MonadError String m
     )
  => FontStore
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
getTextRendering =
  Core.getRendering allocAtlas translate Core.charGlyphAction


newDefaultFontStore
  :: ( MonadIO m
     , MonadError String m
     , Integral i
     )
  => IO (V2 i)
  -> m FontStore
newDefaultFontStore getDims = do
  aw <- makeDefaultAllocateWord getDims
  let dat =
        GlyphRenderingData
        { glyphRenderingDataAllocWord = aw
        , glyphRenderingDataDictMap   = mempty
        , glyphRenderingDataGlyphSet  = S.fromList Core.asciiChars
        }
  Store
    <$> liftIO (atomically $ newTMVar dat)
