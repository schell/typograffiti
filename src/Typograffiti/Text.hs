{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
-- |
-- Module:     Typograffiti.Monad
-- Copyright:  (c) 2018 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- Text rendering abstraction, hiding the details of
-- the Atlas, Cache, and the Harfbuzz library.
module Typograffiti.Text where


import           Control.Concurrent.STM (TMVar, atomically, newTMVar, putTMVar,
                                         readTMVar, takeTMVar)
import           Control.Monad.Except   (MonadError (..), liftEither, runExceptT)
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad          (foldM, forM)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Set               (Set)
import qualified Data.Set               as S
import qualified Data.IntSet            as IS
import           Linear
import qualified Data.ByteString        as B
import           Data.Text.Glyphize     (defaultBuffer, Buffer(..), shape, GlyphInfo(..))
import qualified Data.Text.Glyphize     as HB
import           FreeType.Core.Base
import           Data.Text.Lazy         (Text, pack)
import qualified Data.Text.Lazy         as Txt

import           Typograffiti.Atlas
import           Typograffiti.Cache

data GlyphSize = CharSize Float Float Int Int
               | PixelSize Int Int
               deriving (Show, Eq, Ord)

data SampleText = SampleText {
    sampleFeatures :: [HB.Feature],
    sampleText :: Text,
    tabwidth :: Int
}
defaultSample = SampleText [] (pack $ map toEnum [32..126]) 4
addSampleFeature name value sample@SampleText {..} = sample {
        sampleFeatures =
            HB.Feature (HB.tag_from_string name) value (n*i) (n*succ i) : sampleFeatures
    }
  where
    n = w $ fromEnum $ Txt.length sampleText
    i = w $ length sampleFeatures
    w :: Int -> Word
    w = toEnum

makeDrawText :: (MonadIO m, MonadFail m, MonadError TypograffitiError m,
    MonadIO n, MonadFail n, MonadError TypograffitiError n) =>
    FT_Library -> FilePath -> Int -> GlyphSize -> SampleText ->
    m (String -> [HB.Feature] -> n (AllocatedRendering [TextTransform]))
makeDrawText lib filepath index fontsize SampleText {..} = do
    font <- liftIO $ ft_New_Face lib filepath $ toEnum index
    liftIO $ case fontsize of
        PixelSize w h -> ft_Set_Pixel_Sizes font (toEnum $ x2 w) (toEnum $ x2 h)
        CharSize w h dpix dpiy -> ft_Set_Char_Size font (floor $ 26.6 * 2 * w)
                                                    (floor $ 26.6 * 2 * h)
                                                    (toEnum dpix) (toEnum dpiy)

    bytes <- liftIO $ B.readFile filepath
    let font' = HB.createFont $ HB.createFace bytes $ toEnum index
    let glyphs = map (codepoint . fst) $
            shape font' defaultBuffer {
                text = Txt.replicate (toEnum $ succ $ length sampleFeatures) sampleText
            } sampleFeatures
    let glyphs' = map toEnum $ IS.toList $ IS.fromList $ map fromEnum glyphs
    atlas <- allocAtlas (glyphRetriever font) glyphs'
    liftIO $ ft_Done_Face font

    drawGlyphs <- makeDrawGlyphs
    return $ drawLinesWrapper tabwidth $ \string features ->
        drawGlyphs atlas $ shape font' defaultBuffer { text = pack string } features
  where x2 = (*2)

makeDrawText' a b c d =
    ft_With_FreeType $ \ft -> runExceptT $ makeDrawText ft a b c d

drawLinesWrapper :: (MonadIO m, MonadFail m) =>
    Int -> (String -> [HB.Feature] -> m (AllocatedRendering [TextTransform])) ->
    String -> [HB.Feature] -> m (AllocatedRendering [TextTransform])
drawLinesWrapper indent cb string features = do
    let features' = splitFeatures 0 features $ lines string
    renderers <- mapM (uncurry cb) $ flip zip features' $ map processLine $ lines string
    let drawLine ts wsz y renderer = do
            arDraw renderer (move 0 y:ts) wsz
            let V2 _ height = arSize renderer
            return (y + toEnum height)
    let draw ts wsz = do
            foldM (drawLine ts wsz) 0 renderers
            return ()
    let sizes = map arSize renderers
    let size = V2 (maximum [x | V2 x _ <- sizes]) (sum [y | V2 _ y <- sizes])
    let release = do
            forM renderers arRelease
            return ()
    return AllocatedRendering {
            arDraw = draw,
            arRelease = release,
            arSize = size
          }
  where
    splitFeatures _ [] _ = []
    splitFeatures _ _ [] = []
    splitFeatures offset features' (line:lines') = let n = length line
        in [feat {
                HB.featStart = max 0 (start - offset),
                HB.featEnd = min (toEnum n) (end - offset)
              }
            | feat@HB.Feature {HB.featStart = start, HB.featEnd = end} <- features',
            fromEnum end <= n && end >= offset] :
            splitFeatures (offset + toEnum n) features' lines'

    processLine "" = " " -- enforce nonempty
    processLine cs = expandTabs 0 cs
    -- monospace tabshaping, good enough outside full line-layout.
    expandTabs n cs = case break (== '\t') cs of
        (pre, '\t':cs') -> let spaces = indent - ((length pre + n) `rem` indent)
            in pre ++ replicate spaces ' ' ++ expandTabs (n + length pre + spaces) cs'
        (tail, _) -> tail
