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


import           Control.Concurrent.STM (TMVar, atomically, putTMVar,
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


-- | A pre-rendered bit of text, ready to display given
-- some post compilition transformations. Also contains
-- the text size.
data RenderedGlyphs tfrm m
  = RenderedGlyphs
  { drawRenderedGlyphs   :: tfrm -> m ()
  , sizeOfRenderedGlyphs :: V2 Int
  }


-- | A cache of words and rasterized glyphs.
--
-- * 'tex' is the type of the rasterized bitmap containing glyphs
-- * 'rs' is the resource type the rasterizer maintains
-- * 'tfrm' is the kind of transformation that can be applied to allocated
--   renderings
-- * 'a' is the type of the glyph
data Dictionary tex rs a tfrm
  = Dictionary
  { dictAtlas     :: Atlas tex rs
  , dictWordCache :: WordCache a tfrm tex
  }


-- | All the data needed to render contiguous sets of glyphs quickly.
--
-- * 'tex' is the type of the rasterized bitmap containing glyphs
-- * 'rs' is the resource type the rasterizer maintains
-- * 'tfrm' is the kind of transformation that can be applied to allocated
--   renderings
-- * 'a' is the type of the glyph
-- * 'e' is the type of extended errors that can be thrown, see TypograffitiError
data GlyphRenderingData tex rs tfrm a
  = GlyphRenderingData
  { glyphRenderingDataAllocWord
      :: Atlas tex rs
      -> [a]
      -> IO (Either String (AllocatedRendering tfrm tex))
  -- ^ The operation used to alloc a word.
  -- Generate geometry, use a shader program, set uniforms, etc.
  , glyphRenderingDataDictMap   :: Map (FilePath, GlyphSize) (Dictionary tex rs a tfrm)
  -- ^ The cached fonts.
  , glyphRenderingDataGlyphSet  :: Set a
  -- ^ The default glyph set to have available in all allocated Atlases.
  }


-- | Stored GlyphRenderingData
newtype Store tex rs tfrm a
  = Store
  { unStore :: TMVar (GlyphRenderingData tex rs tfrm a) }


-- | Return
getRendering
  :: ( MonadIO m
     , MonadError String m
     , Ord a
     )
  => (FilePath -> GlyphSize -> [a] -> m (Atlas tex rs))
  -- ^ Monadic action to allocate a fresh Atlas.
  -> (tfrm -> V2 Float -> tfrm)
  -- ^ Pure function for translating a transform.
  -> (a -> GlyphAction)
  -- ^ Pure function for determining the action a glyph has on the renderer.
  -> Store tex rs tfrm a
  -- ^ The dictionary store.
  -> FilePath
  -- ^ The path to the font/glyph file (whatever that may be) to use for rendering.
  -> GlyphSize
  -- ^ The size of the glyphs.
  -> [a]
  -- ^ The glyphs to render.
  -> m (RenderedGlyphs tfrm m)
  -- ^ The rendered glyphs, ready to draw to the screen.
getRendering allocAtlas translate mkAction store file sz str = do
  let mvar = unStore store
  s    <- liftIO $ atomically $ readTMVar mvar
  dict <- case M.lookup (file, sz) $ glyphRenderingDataDictMap s of
    Nothing   -> allocDictionary allocAtlas store file sz
    Just dict -> return dict
  (draw, tsz, cache) <-
    loadWords
      translate
      mkAction
      (\x y -> liftIO (glyphRenderingDataAllocWord s x y) >>= liftEither)
      (dictAtlas dict)
      (dictWordCache dict)
      str
  liftIO
    $ atomically $ do
      s1 <- takeTMVar mvar
      let alterf Nothing      = Just $ Dictionary (dictAtlas dict) cache
          alterf (Just dict1) = Just $ Dictionary (dictAtlas dict1) cache
          fontmap = M.alter alterf (file, sz)
            $ glyphRenderingDataDictMap s1
      putTMVar mvar s1{ glyphRenderingDataDictMap = fontmap }
  return RenderedGlyphs
    { drawRenderedGlyphs   = liftIO . draw
    , sizeOfRenderedGlyphs = tsz
    }


allocDictionary
  :: ( MonadIO m
     , MonadError String m
     , Ord a
     )
  => (FilePath -> GlyphSize -> [a] -> m (Atlas tex rs))
  -> Store tex rs tfrm a
  -> FilePath
  -> GlyphSize
  -> m (Dictionary tex rs a tfrm)
allocDictionary allocAtlas store file sz = do
  let mvar = unStore store
  s     <- liftIO $ atomically $ takeTMVar mvar
  atlas <-
    allocAtlas
      file
      sz
      $ S.toList
      $ glyphRenderingDataGlyphSet s
  let fontmap = glyphRenderingDataDictMap s
      font =
        Dictionary
        { dictAtlas     = atlas
        , dictWordCache = mempty
        }
  liftIO
    $ atomically
    $ putTMVar mvar
    $ s{ glyphRenderingDataDictMap = M.insert (file, sz) font fontmap }
  return font
