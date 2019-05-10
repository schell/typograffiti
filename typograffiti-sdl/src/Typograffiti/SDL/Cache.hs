{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Provides a method of caching rendererd text, making it suitable
-- for interactive rendering.
module Typograffiti.SDL.Cache where

import           Control.Lens               ((^.))
import           Control.Monad              (when)
import           Control.Monad.Except       (MonadError (..), liftEither,
                                             runExceptT)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Bifunctor             (first)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B8
import           Data.Foldable              (for_)
import qualified Data.Vector.Mutable        as MV
import qualified Data.Vector.Unboxed        as UV
import           Data.Word                  (Word8)
import           Foreign.Marshal.Array
import           Linear
import           SDL                        (Point (..), Rectangle (..),
                                             Renderer, Texture, ($=))
import qualified SDL

import qualified Typograffiti               as Core
import           Typograffiti.Atlas         (Atlas (..))
import           Typograffiti.Cache         (AllocatedRendering (..), WordCache)
import           Typograffiti.Transform     (Affine (..), Transform (..))

import           Typograffiti.Freetype      (FT_Face, FT_Library, boundingBox,
                                             makeCharQuad, quadsBounds,
                                             stringQuads)
import           Typograffiti.SDL.Transform (Multiply (..), TextTransform,
                                             translate)


-- | Load the given text into the given WordCache using the given monadic
-- rendering and transform operations.
-- This is a specialized version of Typograffiti.Cache.loadText.
loadText
  :: ( MonadIO m
     , MonadError String m
     )
  => ( Atlas Texture (FT_Library, FT_Face)
       -> String
       -> m (AllocatedRendering [TextTransform] Texture)
     )
  -- ^ Monadic operation used to allocate a word.
  -> Atlas Texture (FT_Library, FT_Face)
  -- ^ The character atlas that holds our letters.
  -> WordCache Char [TextTransform] Texture
  -- ^ The WordCache to load AllocatedRenderings into.
  -> String
  -- ^ The string to render.
  -- This string may contain newlines, which will be respected.
  -> m ([TextTransform] -> IO (), V2 Int, WordCache Char [TextTransform] Texture)
  -- ^ Returns a function for rendering the text, the size of the text and the
  -- new WordCache with the allocated renderings of the text.
loadText = Core.loadWords translate Core.charGlyphAction


transformToUnits
  :: [TextTransform]
  -> (V2 Float, V2 Float, Float, V4 Float)
transformToUnits = foldl toUnit (0, 1, 0, 1)
  where
    toUnit (t, s, r, clr) (Transform (Multiply c)) =
      (t, s, r, clr * c)
    toUnit (t, s, r, c) (TransformAffine a) =
      case a of
        AffineTranslate v -> (t + v, s, r, c)
        AffineScale sc    -> (t, s * sc, r, c)
        AffineRotate rt   -> (t, s, r + rt, c)


liftSDL
  :: ( MonadIO m
     , MonadError String m
     )
  => m (Either String a)
  -> m a
liftSDL n = n >>= liftEither


quad2Rectangle
  :: Integral a
  => (V2 Float, V2 Float)
  -> Rectangle a
quad2Rectangle (tl, br) =
  Rectangle
    (P $ round <$> tl)
    (abs . round <$> br - tl)


-- | A default operation for allocating one word worth of geometry. This is "word" as in
-- an English word, not a data type.
makeDefaultAllocateWord
  :: ( MonadIO m
     , MonadError String m
     )
  => Renderer
  -> m (Atlas Texture (FT_Library, FT_Face)
        -> String
        -> IO (Either String (AllocatedRendering [TextTransform] Texture))
       )
makeDefaultAllocateWord r = return $ \atlas -> \case
  ""     -> return $ Right mempty
  string -> do
    -- Generate our string geometry
    runExceptT (stringQuads atlas True string) >>= \case
      Left err -> return $ Left err
      Right quads -> do
        let (tl, br) = quadsBounds quads
            (sz@(V2 _ szh)) = br - tl
        tex <-
          SDL.createTexture
            r
            SDL.RGBA8888
            SDL.TextureAccessTarget
            (round <$> sz)
        -- Bind the texture as the renderer's target,
        -- draw the word into our texture, then unbind
        SDL.textureBlendMode tex $= SDL.BlendAlphaBlend
        prev <- SDL.get $ SDL.rendererRenderTarget r
        SDL.rendererRenderTarget r $= Just tex
        UV.forM_ quads $ \((destTL, destBR), (srcTL, srcBR)) -> do
          -- The source quads are in percentages of the total texture size,
          -- so we have to convert them
          let cvtSrc = (* (fromIntegral <$> atlasTextureSize atlas))
              srcRect = quad2Rectangle (cvtSrc srcTL, cvtSrc srcBR)
              cvtDest v = V2 (v ^. _x) (sz ^. _y + v ^. _y - br ^. _y)
              destRect = quad2Rectangle (cvtDest destTL, cvtDest destBR)
          SDL.copy
            r
            (atlasTexture atlas)
            (Just srcRect)
            (Just destRect)
        SDL.rendererRenderTarget r $= prev
        -- Return a draw function and a release function
        let draw :: [TextTransform] -> IO ()
            draw ts = do
              let (trns, scl, rot, clr) = transformToUnits ts
                  pos = trns + tl
                  color :: V4 Word8 = floor . (* 255.0) . (/ 1.0) <$> clr
              SDL.textureColorMod tex $= color ^. _xyz
              SDL.textureAlphaMod tex $= color ^. _w
              SDL.copyEx
                r
                tex
                (Just
                   $ SDL.Rectangle 0
                       $ round <$> sz
                )
                (Just
                   $ SDL.Rectangle
                       (SDL.P $ round <$> pos)
                       $ round <$> scl * sz
                )
                (realToFrac rot)
                (Just $ (P $ round <$> pos))
                (V2 False False)
            release = SDL.destroyTexture tex
        return
          $ Right AllocatedRendering
              { arDraw     = draw
              , arTextures = [tex]
              , arRelease  = release
              , arSizes    = [round <$> sz]
              }
