{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Module:     Typograffiti.Cache
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides a method of caching rendererd text, making it suitable
-- for interactive rendering. You can use the defaultCache or provide your own.
--
module Typograffiti.Cache where

import           Control.Monad          (foldM)
import           Control.Monad.Except   (MonadError (..), liftEither)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Bifunctor         (first)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B8
import qualified Data.IntMap            as IM
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Vector.Unboxed    as UV
import           Foreign.Marshal.Array
import           Graphics.GL
import           Linear

import           Typograffiti.Atlas
import           Typograffiti.GL
import           Typograffiti.Glyph


-- | Generic operations for text layout.
class Layout t where
  translate :: t -> V2 Float -> t


-- | Holds an allocated draw function for some amount of text. The function
-- takes one parameter that can be used to transform the text in various ways.
-- This type is generic and can be used to take advantage of your own font
-- rendering shaders.
data AllocatedRendering t m = AllocatedRendering
  { arDraw    :: t -> m ()
    -- ^ Draw the text with some transformation in some monad.
  , arRelease :: m ()
    -- ^ Release the allocated draw function in some monad.
  , arSize    :: V2 Int
    -- ^ The size (in pixels) of the drawn text.
  }


newtype WordCache t m = WordCache
  { unWordCache :: Map String (AllocatedRendering t m) }
  deriving (Semigroup, Monoid)


-- | Load a string of words into the WordCache.
loadWords
  :: Monad m
  => (Atlas -> String -> m (AllocatedRendering t m))
  -- ^ Operation used to allocate a word.
  -> Atlas
  -- ^ The character atlas that holds our letters, which is used to generate
  -- the word geometry.
  -> WordCache t m
  -- ^ The atlas to load the words into.
  -> String
  -- ^ The string of words to load, with each word separated by spaces.
  -> m (WordCache t m)
loadWords f atlas (WordCache cache) str = do
  wm <- foldM loadWord cache (words str)
  return $ WordCache wm
  where loadWord wm word
          | M.member word wm = return wm
          | otherwise = do
              w <- f atlas word
              return $ M.insert word w wm


-- | Unload any words from the cache that are not contained in the source string.
unloadMissingWords
  :: Monad m
  => WordCache t m
  -- ^ The WordCache to unload words from.
  -> String
  -- ^ The source string.
  -> m (WordCache t m)
unloadMissingWords (WordCache cache) str = do
  let ws      = M.fromList $ zip (words str) (repeat ())
      missing = M.difference cache ws
      retain  = M.difference cache missing
  sequence_ $ arRelease <$> missing
  return $ WordCache retain


-- | Constructs a 'Renderer2' from the given color and string. The 'WordMap'
-- record of the given 'Atlas' is used to construct the string geometry, greatly
-- improving performance and allowing longer strings to be compiled and renderered
-- in real time. To create a new 'Atlas' see 'allocAtlas'.
--
-- Note that since word geometries are stored in the 'Atlas' 'WordMap' and multiple
-- renderers can reference the same 'Atlas', the returned 'Renderer2' contains a
-- clean up operation that does nothing. It is expected that the programmer
-- will call 'freeAtlas' manually when the 'Atlas' is no longer needed.
loadText
  :: forall m t. (Monad m, Layout t)
  => (Atlas -> String -> m (AllocatedRendering t m))
  -- ^ Operation used to allocate a word.
  -> Atlas
  -- ^ The character atlas that holds our letters.
  -> WordCache t m
  -- ^ The WordCache to load AllocatedRenderings into.
  -> String
  -- ^ The string to render.
  -- This string may contain newlines, which will be respected.
  -> m (t -> m (), V2 Int, WordCache t m)
  -- ^ Returns a function for rendering the text, the size of the text and the
  -- new WordCache with the allocated renderings of the text.
loadText f atlas wc@(WordCache cache) str = do
  wc1@(WordCache cache1) <- loadWords f atlas wc str
  let glyphw  = round $ pixelWidth $ atlasGlyphSize atlas
      spacew  :: Int
      spacew  = fromMaybe glyphw $ do
        metrcs <- IM.lookup (fromEnum ' ') $ atlasMetrics atlas
        let V2 x _ = glyphAdvance metrcs
        return x
      glyphh = pixelHeight $ atlasGlyphSize atlas
      spaceh = round glyphh
      isWhiteSpace c = c == ' ' || c == '\n' || c == '\t'
      renderWord :: t -> V2 Int -> String -> m ()
      renderWord _ _ ""       = return ()
      renderWord t (V2 _ y) ('\n':cs) = renderWord t (V2 0 (y + spaceh)) cs
      renderWord t (V2 x y) (' ':cs)  = renderWord t (V2 (x + spacew) y) cs
      renderWord t v@(V2 x y) cs               = do
        let word = takeWhile (not . isWhiteSpace) cs
            rest = drop (length word) cs
        case M.lookup word cache1 of
          Nothing -> renderWord t v rest
          Just ar -> do
            let t1 = translate t $ fromIntegral <$> v
                V2 w _ = arSize ar
                pen = V2 (x + fromIntegral w) y
            arDraw ar t1
            renderWord t pen rest
      rr t = renderWord t 0 str
      measureString :: (V2 Int, V2 Int) -> String -> (V2 Int, V2 Int)
      measureString (V2 x y, V2 w h) ""        = (V2 x y, V2 w h)
      measureString (V2 x y, V2 w _) (' ':cs)  =
        let nx = x + spacew in measureString (V2 nx y, V2 (max w nx) y) cs
      measureString (V2 x y, V2 w h) ('\n':cs) =
        let ny = y + spaceh in measureString (V2 x ny, V2 w (max h ny)) cs
      measureString (V2 x y, V2 w h) cs        =
        let word = takeWhile (not . isWhiteSpace) cs
            rest = drop (length word) cs
            n    = case M.lookup word cache of
                     Nothing -> (V2 x y, V2 w h)
                     Just ar -> let V2 ww _ = arSize ar
                                    nx      = x + ww
                                in (V2 nx y, V2 (max w nx) y)
        in measureString n rest
      V2 szw szh = snd $ measureString (0,0) str
  return (rr, V2 szw (max spaceh szh), wc1)


--------------------------------------------------------------------------------
-- Default word allocation
--------------------------------------------------------------------------------


data SpatialTransform = SpatialTransformTranslate (V2 Float)
                      | SpatialTransformScale (V2 Float)
                      | SpatialTransformRotate Float


data TextTransform = TextTransformMultiply (V4 Float)
                   | TextTransformSpatial SpatialTransform


move :: Float -> Float -> TextTransform
move x y =
  TextTransformSpatial
  $ SpatialTransformTranslate
  $ V2 x y


scale :: Float -> Float -> TextTransform
scale x y =
  TextTransformSpatial
  $ SpatialTransformScale
  $ V2 x y


rotate :: Float -> TextTransform
rotate =
  TextTransformSpatial
  . SpatialTransformRotate


color :: Float -> Float -> Float -> Float -> TextTransform
color r g b a =
  TextTransformMultiply
  $ V4 r g b a


alpha :: Float -> TextTransform
alpha =
  TextTransformMultiply
  . V4 1 1 1


instance Layout [TextTransform] where
  translate ts (V2 x y) = ts ++ [move x y]


transformToUniforms
  :: [TextTransform]
  -> (M44 Float, V4 Float)
transformToUniforms = foldl toUniform (identity, 1.0)
  where toUniform (mv, clr) (TextTransformMultiply c) =
          (mv, clr * c)
        toUniform (mv, clr) (TextTransformSpatial s) =
          let mv1 = case s of
                SpatialTransformTranslate (V2 x y) ->
                  mv !*! mat4Translate (V3 x y 0)
                SpatialTransformScale (V2 x y) ->
                  mv !*! mat4Scale (V3 x y 1)
                SpatialTransformRotate r ->
                  mv !*! mat4Rotate r (V3 0 0 1)
          in (mv1, clr)


vertexShader :: ByteString
vertexShader = B8.pack $ unlines
  [ "#version 330 core"
  , "uniform mat4 projection;"
  , "uniform mat4 modelview;"
  , "in vec2 position;"
  , "in vec2 uv;"
  , "out vec2 fuv;"
  , "void main () {"
  , "  fuv = uv;"
  , "  gl_Position = projection * modelview * vec4(position.xy, 0.0, 1.0);"
  , "}"
  ]


fragmentShader :: ByteString
fragmentShader = B8.pack $ unlines
  [ "#version 330 core"
  , "in vec2 fuv;"
  , "out vec4 fcolor;"
  , "uniform sampler2D tex;"
  , "uniform vec4 mult_color;"
  , "void main () {"
  , "  vec4 tcolor = texture(tex, fuv);"
  , "  fcolor = vec4(mult_color.rgb, mult_color.a * tcolor.r);"
  , "}"
  ]


makeDefaultAllocateWord
  :: ( MonadIO m
     , MonadError TypograffitiError m
     , Integral i
     )
  => m (V2 i)
  -- ^ A monadic operation that returns the current context's dimentions.
  -- This is used to set the orthographic projection for rendering text.
  -> m (Atlas -> String -> m (AllocatedRendering [TextTransform] m))
makeDefaultAllocateWord getContextSize = do
  -- Compile our shader program
  let position = 0
      uv       = 1
      liftGL   = liftEither . first TypograffitiErrorGL
  vert <- liftGL =<< compileOGLShader vertexShader GL_VERTEX_SHADER
  frag <- liftGL =<< compileOGLShader fragmentShader GL_FRAGMENT_SHADER
  prog <- liftGL =<< compileOGLProgram
    [ ("position", fromIntegral position)
    , ("uv", fromIntegral uv)
    ]
    [vert, frag]
  glUseProgram prog
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
  -- Get our uniform locations
  pjU    <- getUniformLocation prog "projection"
  mvU    <- getUniformLocation prog "modelview"
  multU  <- getUniformLocation prog "mult_color"
  texU   <- getUniformLocation prog "tex"
  -- Return a function that will generate new words
  return $ \atlas string -> do
    vao   <- newBoundVAO
    pbuf  <- newBuffer
    uvbuf <- newBuffer
    -- Generate our string geometry
    geom <- stringTris atlas True string
    let (ps, uvs) = UV.unzip geom
    -- Buffer the geometry into our attributes
    bufferGeometry position pbuf  ps
    bufferGeometry uv       uvbuf uvs
    glBindVertexArray 0

    let draw ts = do
          let (mv, multVal) = transformToUniforms ts
          glUseProgram prog
          wsz <- getContextSize
          let pj :: M44 Float = orthoProjection wsz
          updateUniform prog pjU pj
          updateUniform prog mvU  mv
          updateUniform prog multU multVal
          updateUniform prog texU (0 :: Int)
          glBindVertexArray vao
          withBoundTextures [atlasTexture atlas] $ do
            drawVAO
              prog
              vao
              GL_TRIANGLES
              (fromIntegral $ UV.length ps)
            glBindVertexArray 0

        release = liftIO $ do
          withArray [pbuf, uvbuf] $ glDeleteBuffers 2
          withArray [vao] $ glDeleteVertexArrays 1
        (tl, br) = boundingBox ps

        size = br - tl

    return AllocatedRendering
      { arDraw    = draw
      , arRelease = release
      , arSize    = round <$> size
      }
