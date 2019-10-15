module Art.Grammar where

import Data.List.NonEmpty
import Art.Geometry

data Modifier
  = Color String
  | Scale Float
  | Move Vec

type Production = (Int, [Modifier], NonEmpty Symbol)

data Symbol
  = NonTerminal (NonEmpty Production)

  -- center is at rel(0, 0)
  | Circle Float

  -- points and polys start at rel(0, 0)
  | Poly [Vec]
