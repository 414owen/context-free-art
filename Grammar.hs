module Grammar where

import Data.List.NonEmpty
import Geometry

data Modifier
  = Color String
  | Stroke Float
  | Scale Float
  | Move Vec

type Production = (Int, [Modifier], NonEmpty Symbol)

data Symbol
  = NonTerminal (NonEmpty Production)

  -- center is at rel(0, 0)
  | Circle Float

  -- points and polys start at rel(0, 0)
  | Poly [Vec]
