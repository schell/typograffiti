-- |
-- Module:     Typograffiti.Rich
-- Copyright:  (c) 2023 Adrian Cochrane
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--             & Adrian Cochrane <alcinnz@argonaut-constellation.org>
--
-- Abstraction for building richtext strings to be rendered via Typograffiti.
--
module Typograffiti.Rich where
import           Data.Text.Lazy     (Text, append, pack)
import qualified Data.Text.Lazy as  Txt
import           Data.Text.Glyphize (Feature(..), tag_from_string, parseFeature)
import           Data.String        (IsString(..))
import           Data.Word          (Word32)

-- | Retreives the length of some text as a `Word` suitable for storing in a `Feature`.
length' :: Text -> Word
length' = toEnum . fromEnum . Txt.length

-- | Styled text to be rendered.
data RichText = RichText {
    text :: Text,
    features :: [Feature]
}

instance IsString RichText where
    fromString x = flip RichText [] $ pack x
-- | Converts a `String` to renderable `RichText`.
str :: String -> RichText
str = fromString
-- | Converts `Text` to renderable `RichText`.
txt :: Text -> RichText
txt = flip RichText []

-- | Concatenate richtext data.
($$) :: RichText -> RichText -> RichText
RichText ltext lfeat $$ RichText rtext rfeat = RichText {
    text = append ltext rtext,
    features = let n = length' ltext in lfeat ++ [
        feat { featStart = start + n, featEnd = end + n }
        | feat@Feature { featStart = start, featEnd = end } <- rfeat]
  }

-- | Applies the given OpenType Feature to the given `RichText`.
-- Check your font for details on which OpenType features are supported.
-- Or see https://learn.microsoft.com/en-us/typography/opentype/spec/featurelist/
-- (from which much of this documentation is taken).
style :: String -> Word32 -> RichText -> RichText
style feat value (RichText text feats) = RichText {
    text = text,
    features = Feature (tag_from_string feat) value 0 (length' text) : feats
  }
-- | Parses the given syntax akin to CSS font-feature-settings & apply to
-- The given RichText.
apply :: String -> RichText -> RichText
apply syntax rich | Just feat <- parseFeature syntax = rich {
      features = feat { featStart = 0, featEnd = length' $ text rich } : features rich
    }
  | otherwise = rich

alt, case_, centerCJKPunct, capSpace, ctxtSwash, petiteCaps', smallCaps', expertJ,
    finGlyph, fract, fullWidth, hist, hkana, histLig, hojo, halfWidth, italic,
    justifyAlt, jap78, jap83, jap90, jap04, kerning, lBounds, liningFig, localized,
    mathGreek, altAnnotation, nlcKanji, oldFig, ordinals, ornament, propAltWidth,
    petiteCaps, propKana, propFig, propWidth, quarterWidth, rand, rBounds, ruby,
    styleAlt, sciInferior, smallCaps, simpleCJ, subscript, superscript, swash,
    titling, traditionNameJ, tabularFig, traditionCJ, thirdWidth, unicase, vAlt,
    vert, vHalfAlt, vKanaAlt, vKerning, vPropAlt, vRotAlt, vrot,
    slash0 :: Word32 -> RichText -> RichText
altFrac, ctxtAlt, ctxtLig, optLigs, lig :: Bool -> RichText -> RichText
-- | This feature makes all variations of a selected character accessible.
-- This serves several purposes: An application may not support the feature by
-- which the desired glyph would normally be accessed; the user may need a glyph
-- outside the context supported by the normal substitution, or the user may not
-- know what feature produces the desired glyph. Since many-to-one substitutions
-- are not covered, ligatures would not appear in this table unless they were
-- variant forms of another ligature.
alt         = style "aalt"
-- | Replaces figures separated by a slash with an alternative form.
altFrac True= style "afrc" 4
altFrac False=style "afrc" 0
-- | n specified situations, replaces default glyphs with alternate forms which
-- provide better joining behavior. Used in script typefaces which are designed
-- to have some or all of their glyphs join.
ctxtAlt True= style "calt" 6
ctxtAlt False=style "calt" 0
-- | Shifts various punctuation marks up to a position that works better with
-- all-capital sequences or sets of lining figures; also changes oldstyle
-- figures to lining figures. By default, glyphs in a text face are designed to
-- work with lowercase characters. Some characters should be shifted vertically
-- to fit the higher visual center of all-capital or lining text. Also, lining
-- figures are the same height (or close to it) as capitals, and fit much better
-- with all-capital text.
case_       = style "case"
-- | Replaces a sequence of glyphs with a single glyph which is preferred for
-- typographic purposes. Unlike other ligature features, 'clig' specifies the
-- context in which the ligature is recommended. This capability is important
-- in some script designs and for swash ligatures.
ctxtLig True= style "clig" 8
ctxtLig False=style "clig" 0
-- | Centers specific punctuation marks for those fonts that do not include
-- centered and non-centered forms.
centerCJKPunct = style "cpct"
-- | Globally adjusts inter-glyph spacing for all-capital text. Most typefaces
-- contain capitals and lowercase characters, and the capitals are positioned to
-- work with the lowercase. When capitals are used for words, they need more
-- space between them for legibility and esthetics. This feature would not apply
-- to monospaced designs. Of course the user may want to override this behavior
-- in order to do more pronounced letterspacing for esthetic reasons.
capSpace    = style "cpsp"
-- | This feature replaces default character glyphs with corresponding swash
-- glyphs in a specified context. Note that there may be more than one swash
-- alternate for a given character.
ctxtSwash   = style "cswh"
-- | This feature turns capital characters into petite capitals. It is generally
-- used for words which would otherwise be set in all caps, such as acronyms,
-- but which are desired in petite-cap form to avoid disrupting the flow of text.
-- See the 'pcap' feature description for notes on the relationship of caps,
-- smallcaps and petite caps.
petiteCaps' = style "c2pc"
-- | This feature turns capital characters into small capitals. It is generally
-- used for words which would otherwise be set in all caps, such as acronyms,
-- but which are desired in small-cap form to avoid disrupting the flow of text.
smallCaps'  = style "c2sc"
-- | Replaces a sequence of glyphs with a single glyph which is preferred for
-- typographic purposes. This feature covers those ligatures which may be used
-- for special effect, at the user’s preference.
optLigs True= style "dlig" 4
optLigs False=style "dlig" 0
-- | Like the JIS78 Forms feature, this feature replaces standard forms in
-- Japanese fonts with corresponding forms preferred by typographers. Although
-- most of the JIS78 substitutions are included, the expert substitution goes on
-- to handle many more characters.
expertJ     = style "expt"
-- | Replaces line final glyphs with alternate forms specifically designed for
-- this purpose (they would have less or more advance width as need may be), to
-- help justification of text.
finGlyph    = style "falt"
-- | Replaces figures (digits) separated by a slash (U+002F or U+2044) with
-- “common” (diagonal) fractions.
fract       = style "frac"
-- | Replaces glyphs set on other widths with glyphs set on full (usually em)
-- widths. In a CJKV font, this may include “lower ASCII” Latin characters and
-- various symbols. In a European font, this feature replaces proportionally-spaced
-- glyphs with monospaced glyphs, which are generally set on widths of 0.6 em.
fullWidth   = style "fwid"
-- | Some letterforms were in common use in the past, but appear anachronistic
-- today. The best-known example is the long form of s; others would include the
-- old Fraktur k. Some fonts include the historical forms as alternates, so they
-- can be used for a “period” effect. This feature replaces the default (current)
-- forms with the historical alternates. While some ligatures are also used for
-- historical effect, this feature deals only with single characters.
hist        = style "hist"
-- | Replaces standard kana with forms that have been specially designed for only
-- horizontal writing. This is a typographic optimization for improved fit and
-- more even color. Also see 'vkana'.
hkana       = style "hkna"
-- | Some ligatures were in common use in the past, but appear anachronistic today.
-- Some fonts include the historical forms as alternates, so they can be used for
-- a “period” effect. This feature replaces the default (current) forms with the
-- historical alternates.
histLig        = style "hlig"
-- | The JIS X 0212-1990 (aka, “Hojo Kanji”) and JIS X 0213:2004 character sets
-- overlap significantly. In some cases their prototypical glyphs differ. When
-- building fonts that support both JIS X 0212-1990 and JIS X 0213:2004 (such as
-- those supporting the Adobe-Japan 1-6 character collection), it is recommended
-- that JIS X 0213:2004 forms be preferred as the encoded form. The 'hojo'
-- feature is used to access the JIS X 0212-1990 glyphs for the cases when the
-- JIS X 0213:2004 form is encoded.
hojo        = style "hojo"
-- | Replaces glyphs on proportional widths, or fixed widths other than half an
-- em, with glyphs on half-em (en) widths. Many CJKV fonts have glyphs which are
-- set on multiple widths; this feature selects the half-em version. There are
-- various contexts in which this is the preferred behavior, including
-- compatibility with older desktop documents.
halfWidth   = style "hwid"
-- | Some fonts (such as Adobe’s Pro Japanese fonts) will have both Roman and
-- Italic forms of some characters in a single font. This feature replaces the
-- Roman glyphs with the corresponding Italic glyphs.
italic      = style "ital"
-- | Improves justification of text by replacing glyphs with alternate forms
-- specifically designed for this purpose (they would have less or more advance
-- width as need may be).
justifyAlt  = style "jalt"
-- | This feature replaces default (JIS90) Japanese glyphs with the corresponding
-- forms from the JIS C 6226-1978 (JIS78) specification.
jap78       = style "jp78"
-- | This feature replaces default (JIS90) Japanese glyphs with the corresponding
-- forms from the JIS X 0208-1983 (JIS83) specification.
jap83       = style "jp83"
-- | This feature replaces Japanese glyphs from the JIS78 or JIS83 specifications
-- with the corresponding forms from the JIS X 0208-1990 (JIS90) specification.
jap90       = style "jp90"
-- | The National Language Council (NLC) of Japan has defined new glyph shapes
-- for a number of JIS characters, which were incorporated into JIS X 0213:2004
-- as new prototypical forms. The 'jp04' feature is a subset of the 'nlck'
-- feature, and is used to access these prototypical glyphs in a manner that
-- maintains the integrity of JIS X 0213:2004.
jap04       = style "jp04"
-- | Adjusts amount of space between glyphs, generally to provide optically
-- consistent spacing between glyphs. Although a well-designed typeface has
-- consistent inter-glyph spacing overall, some glyph combinations require
-- adjustment for improved legibility. Besides standard adjustment in the
-- horizontal direction, this feature can supply size-dependent kerning data
-- via device tables, “cross-stream” kerning in the Y text direction, and
-- adjustment of glyph placement independent of the advance adjustment. Note
-- that this feature may apply to runs of more than two glyphs, and would not
-- be used in monospaced fonts. Also note that this feature does not apply to
-- text set vertically.
kerning     = style "kern"
-- | Aligns glyphs by their apparent left extents at the left ends of horizontal
-- lines of text, replacing the default behavior of aligning glyphs by their origins.
lBounds     = style "lfbd"
-- | Replaces a sequence of glyphs with a single glyph which is preferred for
-- typographic purposes. This feature covers the ligatures which the
-- designer or manufacturer judges should be used in normal conditions.
lig True    = style "liga" 4
lig False   = style "liga" 0
-- | This feature changes selected non-lining figures (digits) to lining figures.
liningFig   = style "lnum"
-- | Many scripts used to write multiple languages over wide geographical areas
-- have developed localized variant forms of specific letters, which are used by
-- individual literary communities. For example, a number of letters in the
-- Bulgarian and Serbian alphabets have forms distinct from their Russian
-- counterparts and from each other. In some cases the localized form differs
-- only subtly from the script “norm”, in others the forms are radically distinct.
-- This feature enables localized forms of glyphs to be substituted for default forms.
localized   = style "locl"
-- | Replaces standard typographic forms of Greek glyphs with corresponding forms
-- commonly used in mathematical notation (which are a subset of the Greek alphabet).
mathGreek   = style "mgrk"
-- | Replaces default glyphs with various notational forms (e.g. glyphs placed
-- in open or solid circles, squares, parentheses, diamonds or rounded boxes).
-- In some cases an annotation form may already be present, but the user may want
-- a different one.
altAnnotation=style "nalt"
-- | The National Language Council (NLC) of Japan has defined new glyph shapes
-- for a number of JIS characters in 2000.
nlcKanji    = style "nlck"
-- | This feature changes selected figures from the default or lining style to
-- oldstyle form.
oldFig      = style "onum"
-- | Replaces default alphabetic glyphs with the corresponding ordinal forms for
-- use after figures. One exception to the follows-a-figure rule is the numero
-- character (U+2116), which is actually a ligature substitution, but is best
-- accessed through this feature.
ordinals    = style "ordn"
-- | This is a dual-function feature, which uses two input methods to give the
-- user access to ornament glyphs (e.g. fleurons, dingbats and border elements)
-- in the font. One method replaces the bullet character with a selection from
-- the full set of available ornaments; the other replaces specific “lower ASCII”
-- characters with ornaments assigned to them. The first approach supports the
-- general or browsing user; the second supports the power user.
ornament    = style "ornm"
-- | Re-spaces glyphs designed to be set on full-em widths, fitting them onto
-- individual (more or less proportional) horizontal widths. This differs from
-- 'pwid' in that it does not substitute new glyphs (GPOS, not GSUB feature).
-- The user may prefer the monospaced form, or may simply want to ensure that
-- the glyph is well-fit and not rotated in vertical setting (Latin forms
-- designed for proportional spacing would be rotated).
propAltWidth= style "palt"
-- | Some fonts contain an additional size of capital letters, shorter than the
-- regular smallcaps and whimsically referred to as petite caps. Such forms are
-- most likely to be found in designs with a small lowercase x-height, where they
-- better harmonise with lowercase text than the taller smallcaps (for examples
-- of petite caps, see the Emigre type families Mrs Eaves and Filosofia). This
-- feature turns glyphs for lowercase characters into petite capitals. Forms
-- related to petite capitals, such as specially designed figures, may be included.
petiteCaps  = style "pcap"
-- | Replaces glyphs, kana and kana-related, set on uniform widths (half or
-- full-width) with proportional glyphs.
propKana    = style "pkna"
-- | Replaces figure glyphs set on uniform (tabular) widths with corresponding
-- glyphs set on glyph-specific (proportional) widths. Tabular widths will
-- generally be the default, but this cannot be safely assumed. Of course this
-- feature would not be present in monospaced designs.
propFig     = style "pnum"
-- | Replaces glyphs set on uniform widths (typically full or half-em) with
-- proportionally spaced glyphs. The proportional variants are often used for the
-- Latin characters in CJKV fonts, but may also be used for Kana in Japanese fonts.
propWidth   = style "pwid"
-- | Replaces glyphs on other widths with glyphs set on widths of one quarter
-- of an em (half an en). The characters involved are normally figures and
-- some forms of punctuation.
quarterWidth= style "qwid"
-- | In order to emulate the irregularity and variety of handwritten text, this
-- feature allows multiple alternate forms to be used.
rand        = style "rand"
-- | Aligns glyphs by their apparent right extents at the right ends of horizontal
-- lines of text, replacing the default behavior of aligning glyphs by their origins.
rBounds     = style "rtbd"
-- | Japanese typesetting often uses smaller kana glyphs, generally in
-- superscripted form, to clarify the meaning of kanji which may be unfamiliar
-- to the reader. These are called “ruby”, from the old typesetting term for
-- four-point-sized type. This feature identifies glyphs in the font which have
-- been designed for this use, substituting them for the default designs.
ruby        = style "ruby"
-- | Many fonts contain alternate glyph designs for a purely esthetic effect;
-- these don’t always fit into a clear category like swash or historical. As in
-- the case of swash glyphs, there may be more than one alternate form. This
-- feature replaces the default forms with the stylistic alternates.
styleAlt    = style "salt"
-- | Replaces lining or oldstyle figures (digits) with inferior figures (smaller
-- glyphs which sit lower than the standard baseline, primarily for chemical or
-- mathematical notation). May also replace glyphs for lowercase characters with
-- alphabetic inferiors.
sciInferior = style "sinf"
-- | This feature turns glyphs for lowercase characters into small capitals. It
-- is generally used for display lines set in Large & small caps, such as titles.
-- Forms related to small capitals, such as oldstyle figures, may be included.
smallCaps   = style "smcp"
-- | Replaces “traditional” Chinese or Japanese forms with the corresponding
-- “simplified” forms.
simpleCJ    = style "smpl"
-- | The 'subs' feature may replace a default glyph with a subscript glyph, or it
-- may combine a glyph substitution with positioning adjustments for proper placement.
subscript   = style "subs"
-- | Replaces lining or oldstyle figures with superior figures (primarily for
-- footnote indication), and replaces lowercase letters with superior letters
-- (primarily for abbreviated French titles).
superscript = style "sups"
-- | This feature replaces default character glyphs with corresponding swash glyphs.
-- Note that there may be more than one swash alternate for a given character.
swash       = style "swsh"
-- | This feature replaces the default glyphs with corresponding forms designed
-- specifically for titling. These may be all-capital and\/or larger on the body,
-- and adjusted for viewing at larger sizes.
titling     = style "titl"
-- | Replaces “simplified” Japanese kanji forms with the corresponding
-- “traditional” forms. This is equivalent to the Traditional Forms feature,
-- but explicitly limited to the traditional forms considered proper for use
-- in personal names (as many as 205 glyphs in some fonts).
traditionNameJ = style "tnam"
-- | Replaces figure glyphs set on proportional widths with corresponding glyphs
-- set on uniform (tabular) widths. Tabular widths will generally be the default,
-- but this cannot be safely assumed. Of course this feature would not be present
-- in monospaced designs.
tabularFig  = style "tnum"
-- | Replaces 'simplified' Chinese hanzi or Japanese kanji forms with the
-- corresponding 'traditional' forms.
traditionCJ = style "trad"
-- | Replaces glyphs on other widths with glyphs set on widths of one third of an
-- em. The characters involved are normally figures and some forms of punctuation.
thirdWidth  = style "twid"
-- | This feature maps upper- and lowercase letters to a mixed set of lowercase
-- and small capital forms, resulting in a single case alphabet (for an example
-- of unicase, see the Emigre type family Filosofia). The letters substituted
-- may vary from font to font, as appropriate to the design. If aligning to the
-- x-height, smallcap glyphs may be substituted, or specially designed unicase
-- forms might be used. Substitutions might also include specially designed figures.
unicase     = style "unic"
-- | Repositions glyphs to visually center them within full-height metrics, for
-- use in vertical setting. Applies to full-width Latin, Greek, or Cyrillic
-- glyphs, which are typically included in East Asian fonts, and whose glyphs
-- are aligned on a common horizontal baseline and not rotated relative to the
-- page or text frame.
vAlt        = style "valt"
-- | Transforms default glyphs into glyphs that are appropriate for upright
-- presentation in vertical writing mode. While the glyphs for most characters
-- in East Asian writing systems remain upright when set in vertical writing
-- mode, some must be transformed — usually by rotation, shifting, or different
-- component ordering — for vertical writing mode.
vert        = style "vert"
-- | Re-spaces glyphs designed to be set on full-em heights, fitting them onto
-- half-em heights. This differs from 'valt', which repositions a glyph but does
-- not affect its advance.
vHalfAlt    = style "vhal"
-- | Replaces standard kana with forms that have been specially designed for only
-- vertical writing. This is a typographic optimization for improved fit and more
-- even color. Also see 'hkna'.
vKanaAlt    = style "vkna"
-- | Adjusts amount of space between glyphs, generally to provide optically
-- consistent spacing between glyphs. Although a well-designed typeface has
-- consistent inter-glyph spacing overall, some glyph combinations require
-- adjustment for improved legibility. Besides standard adjustment in the
-- vertical direction, this feature can supply size-dependent kerning data
-- via device tables, “cross-stream” kerning in the X text direction, and
-- adjustment of glyph placement independent of the advance adjustment. Note
-- that this feature may apply to runs of more than two glyphs, and would not
-- be used in monospaced fonts. Also note that this feature applies only to
-- text set vertically.
vKerning    = style "vkrn"
-- | Re-spaces glyphs designed to be set on full-em heights, fitting them onto
-- individual (more or less proportional) vertical heights. This differs from
-- 'valt', which repositions a glyph but does not affect its advance.
vPropAlt    = style "vpal"
-- | Replaces some fixed-width (half-, third- or quarter-width) or
-- proportional-width glyphs (mostly Latin or katakana) with forms suitable for
-- vertical writing (that is, rotated 90 degrees clockwise). Note that these are
-- a superset of the glyphs covered in the 'vert' table.
vRotAlt     = style "vrt2"
-- | Transforms default glyphs into glyphs that are appropriate for sideways
-- presentation in vertical writing mode. While the glyphs for most characters
-- in East Asian writing systems remain upright when set in vertical writing mode,
-- glyphs for other characters — such as those of other scripts or for particular
-- Western-style punctuation — are expected to be presented sideways in vertical writing.
vrot        = style "vrtr"
-- | Some fonts contain both a default form of zero, and an alternative form
-- which uses a diagonal slash through the counter. Especially in condensed
-- designs, it can be difficult to distinguish between 0 and O (zero and capital
-- O) in any situation where capitals and lining figures may be arbitrarily mixed.
-- This feature allows the user to change from the default 0 to a slashed form.
slash0      = style "zero"

off, on, alternate :: Word32
-- | Typical word to turn a font-feature off.
off = 0
-- | Typical word to turn a font-feature on
on = 1
-- | Typical word to switch to the alternate setting for a font-feature.
alternate = 3
