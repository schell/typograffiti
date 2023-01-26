module Typograffiti.Rich where
import           Data.Text.Lazy     (Text, append, pack)
import qualified Data.Text.Lazy as  Txt
import           Data.Text.Glyphize (Feature(..), tag_from_string, parseFeature)
import           Data.String        (IsString(..))
import           Data.Word          (Word32)

length' = toEnum . fromEnum . Txt.length

data RichText = RichText {
    text :: Text,
    features :: [Feature]
}

instance IsString RichText where
    fromString x = flip RichText [] $ pack x
str :: String -> RichText
str = fromString
txt :: Text -> RichText
txt = flip RichText []

($$) :: RichText -> RichText -> RichText
RichText ltext lfeat $$ RichText rtext rfeat = RichText {
    text = append ltext rtext,
    features = let n = length' ltext in lfeat ++ [
        feat { featStart = start + n, featEnd = end + n }
        | feat@Feature { featStart = start, featEnd = end } <- rfeat]
  }

style :: String -> Word32 -> RichText -> RichText
style feat value (RichText text feats) = RichText {
    text = text,
    features = Feature (tag_from_string feat) value 0 (length' text) : feats
  }
apply :: String -> RichText -> RichText
apply syntax rich | Just feat <- parseFeature syntax = rich {
      features = feat { featStart = 0, featEnd = length' $ text rich } : features rich
    }
  | otherwise = rich

alt, case_, centerCJKPunct, capSpace, ctxtSwash, petiteCaps', smallCaps', expertJ,
    finGlyph, fract, fullWidth, hist, hkana, hlig, hojo, halfWidth, italic, justifyAlt,
    jap78, jap83, jap90, jap04, kerning, lBounds, liningFig, localized, mathGreek,
    altAnnotation, nlcKanji, oldFig, ordinals, ornament, propAltWidth, petiteCaps,
    propKana, propFig, propWidth, quarterWidth, rBounds, ruby, styleAlt, sciInferior,
    smallCaps, simpleCJ, subscript, superscript, swash, titling, traditionNameJ,
    tabularFig, traditionCJ, thirdWidth, unicase, vAlt, vert, vHalfAlt, vKanaAlt,
    vKerning, vPropAlt, vRotAlt, vrot, slash0 :: Word32 -> RichText -> RichText
altFrac, ctxtAlt, ctxtLig, optLigs, lig, rand :: Bool -> RichText -> RichText
alt         = style "aalt"
altFrac True= style "afrc" 4
altFrac False=style "afrc" 0
ctxtAlt True= style "calt" 6
ctxtAlt False=style "calt" 0
case_       = style "case"
ctxtLig True= style "clig" 8
ctxtLig False=style "clig" 0
centerCJKPunct = style "cpct"
capSpace    = style "cpsp"
ctxtSwash   = style "cswh"
petiteCaps' = style "c2pc"
smallCaps'  = style "c2sc"
optLigs True= style "dlig" 4
optLigs False=style "dlig" 0
expertJ     = style "expt"
finGlyph    = style "falt"
fract       = style "frac"
fullWidth   = style "fwid"
hist        = style "hist"
hkana       = style "hkna"
hlig        = style "hlig"
hojo        = style "hojo"
halfWidth   = style "hwid"
italic      = style "ital"
justifyAlt  = style "jalt"
jap78       = style "jp78"
jap83       = style "jp83"
jap90       = style "jp90"
jap04       = style "jp04"
kerning     = style "kern"
lBounds     = style "lfbd"
lig True    = style "liga" 4
lig False   = style "liga" 0
liningFig   = style "lnum"
localized   = style "locl"
mathGreek   = style "mgrk"
altAnnotation=style "nalt"
nlcKanji    = style "nlck"
oldFig      = style "onum"
ordinals    = style "ordn"
ornament    = style "ornm"
propAltWidth= style "palt"
petiteCaps  = style "pcap"
propKana    = style "pkna"
propFig     = style "pnum"
propWidth   = style "pwid"
quarterWidth= style "qwid"
rand True   = style "rand" 3
rand False  = style "rand" 0
rBounds     = style "rtbd"
ruby        = style "ruby"
styleAlt    = style "salt"
sciInferior = style "sinf"
smallCaps   = style "smcp"
simpleCJ    = style "smpl"
subscript   = style "subs"
superscript = style "sups"
swash       = style "swsh"
titling     = style "titl"
traditionNameJ = style "tnam"
tabularFig  = style "tnum"
traditionCJ = style "trad"
thirdWidth  = style "twid"
unicase     = style "unic"
vAlt        = style "valt"
vert        = style "vert"
vHalfAlt    = style "vhal"
vKanaAlt    = style "vkna"
vKerning    = style "vkrn"
vPropAlt    = style "vpal"
vRotAlt     = style "vrt2"
vrot        = style "vrtr"
slash0      = style "zero"

off, on, alternate :: Word32
off = 0
on = 1
alternate = 3
