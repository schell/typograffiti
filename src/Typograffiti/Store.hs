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

data GlyphSize = CharSize Float Float Int Int
               | PixelSize Int Int
               deriving (Show, Eq, Ord)

makeDrawTextIndented lib filepath index fontsize features sampletext indent = do
    font <- ft_New_Face lib filepath index
    case fontsize of
        PixelSize w h -> ft_Set_Pixel_Sizes font (toEnum $ x2 w) (toEnum $ x2 h)
        CharSize w h dpix dpiy -> ft_Set_Char_Size font (floor $ 26.6 * 2 * w)
                                                    (floor $ 26.6 * 2 * h)
                                                    (toEnum dpix) (toEnum dpiy)

    bytes <- B.readFile filepath
    let font' = createFont $ createFace bytes $ toEnum $ fromEnum index
    let glyphs = map (codepoint . fst) $
            shape font' defaultBuffer { text = sampletext } features
    let glyphs' = map toEnum $ IS.toList $ IS.fromList $ map fromEnum glyphs
    atlas <- allocAtlas (glyphRetriever font) glyphs'
    ft_Done_Face font

    drawGlyphs <- makeDrawGlyphs
    return $ drawLinesWrapper $ \string ->
        drawGlyphs atlas $ shape font' defaultBuffer { text = string } features
  where x2 = (*2)

makeDrawTextIndented' a b c d e f =
    ft_With_FreeType $ \ft -> makeDrawText ft a b c d e f

makeDrawText a b c d e f = makeDrawTextIndented a b c d e f
makeDrawText' a b c d e = ft_With_FreeType $ \ft -> makeDrawText ft a b c d e

-- Note: May glitch upon ligatures.
makeDrawAsciiIndented a b c d e f =
    makeDrawTextIndented a b c d e (map toEnum [32..126]) f
makeDrawAsciiIndented' a b c d e =
    ft_With_FreeType $ \ft -> makeDrawAsciiIndented ft a b c d e
makeDrawAscii a b c d e = makeDrawText a b c d e $ map toEnum [32..126]
makeDrawAscii' a b c d = ft_With_FreeType $ \ft -> makeDrawAscii ft a b c d

drawLinesWrapper indent cb string = do
    renderers <- mapM cb $ map processLine $ lines string
    let drawLine ts wsz y renderer = do
        arDraw renderer (move 0 y:ts) wsz
        let V2 _ height = arSize renderer
        return y + height
    let draw ts wsz = do
        foldM (drawLine ts wsz) 0 renderers
        return ()
    let sizes = map arSize renderers
    let size = V2 (max [x | V2 x _ <- sizes]) (sum [y | V2 _ y <- sizes])
    let release = do
        forM renderers arRelease
        return ()
    return AllocatedRendering {
            arDraw = draw,
            arRelease = release,
            arSize = size
          }
  where
    processLine "" = " " -- enforce nonempty
    processLine cs = expandTabs cs
    -- monospace tabshaping, good enough outside full line-layout.
    expandTabs n cs = case break (== '\t') of
        (pre, '\t':cs') -> let spaces = indent - ((length pre + n) `rem` indent)
            in pre ++ replicate spaces ' ' ++ expandTabs (n + length pre + spaces) cs'
        (tail, _) -> tail

-- Add cache of imported fonts
--- Key by filepath & index
--- Maps to Harfbuzz & FreeType fonts,
--- as well as a list of atlases associated with glyphsets & fontfeatures.
