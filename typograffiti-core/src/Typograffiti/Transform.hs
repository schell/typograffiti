-- | Types for transforming allocated renderings.
module Typograffiti.Transform where

import           Linear (V2 (..))


data Affine
  = AffineTranslate (V2 Float)
  | AffineScale (V2 Float)
  | AffineRotate Float


data Transform a
  = TransformAffine Affine
  | Transform a


moveV2 :: V2 Float -> Transform a
moveV2 =
  TransformAffine
  . AffineTranslate


move :: Float -> Float -> Transform a
move x y =
  TransformAffine
  $ AffineTranslate
  $ V2 x y


scaleV2 :: V2 Float -> Transform a
scaleV2 =
  TransformAffine
  . AffineScale


scale :: Float -> Float -> Transform a
scale x y =
  TransformAffine
  $ AffineScale
  $ V2 x y


rotate :: Float -> Transform a
rotate =
  TransformAffine
  . AffineRotate
