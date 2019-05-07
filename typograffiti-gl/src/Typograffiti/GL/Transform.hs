module Typograffiti.GL.Transform where

import           Linear                 (V2 (..), V4 (..))

import           Typograffiti.Transform (Transform (..), move)


newtype Multiply
  = Multiply (V4 Float)


type TextTransform
  = Transform Multiply


color :: Float -> Float -> Float -> Float -> TextTransform
color r g b a =
  Transform
  $ Multiply
  $ V4 r g b a


colorV4 :: V4 Float -> TextTransform
colorV4 =
  Transform
  . Multiply


alpha :: Float -> TextTransform
alpha =
  Transform
  . Multiply
  . V4 1 1 1


translate :: [TextTransform] -> V2 Float -> [TextTransform]
translate ts (V2 x y) = ts ++ [move x y]
