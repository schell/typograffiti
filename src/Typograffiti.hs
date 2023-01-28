-- |
-- Module:     Typograffiti
-- Copyright:  (c) 2018 Schell Scivally, 2023 Adrian Cochrane
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--             & Adrian Cochrane <alcinnz@argonaut-constellation.org>
--
-- This module provides easy freetype2 & Harfbuzz based font rendering with a
-- nice Haskell interface, whilst exposing low-level APIs for those who need it.
module Typograffiti(
    TypograffitiError(..),
    allocAtlas, freeAtlas, stringTris, Atlas(..), GlyphMetrics(..),
    makeDrawGlyphs, AllocatedRendering(..), Layout(..),
    SpatialTransform(..), TextTransform(..), move, scale, rotate, skew, color, alpha,
    withFontStore, newFontStore, FontStore(..), Font(..),
    SampleText (..), defaultSample, addSampleFeature, parseSampleFeature, parseSampleFeatures,
        addFontVariant, parseFontVariant, parseFontVariants,
        varItalic, varOptSize, varSlant, varWidth, varWeight,
    RichText (..), str, txt, ($$), style, apply, on, off, alternate,
        alt, case_, centerCJKPunct, capSpace, ctxtSwash, petiteCaps', smallCaps',
        expertJ, finGlyph, fract, fullWidth, hist, hkana, histLig, hojo, halfWidth,
        italic, justifyAlt, jap78, jap83, jap90, jap04, kerning, lBounds, liningFig,
        localized, mathGreek, altAnnotation, nlcKanji, oldFig, ordinals, ornament,
        propAltWidth, petiteCaps, propKana, propFig, propWidth, quarterWidth,
        rBounds, ruby, styleAlt, sciInferior, smallCaps, simpleCJ, subscript,
        superscript, swash, titling, traditionNameJ, tabularFig, traditionCJ,
        thirdWidth, unicase, vAlt, vert, vHalfAlt, vKanaAlt, vKerning, vPropAlt,
        vRotAlt, vrot, slash0, altFrac, ctxtAlt, ctxtLig, optLigs, lig, rand,
    GlyphSize(..), makeDrawTextCached, makeDrawText, makeDrawText'
) where

import Typograffiti.Atlas
import Typograffiti.Cache
import Typograffiti.Store
import Typograffiti.Text
import Typograffiti.Rich
