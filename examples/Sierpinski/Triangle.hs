{-# LANGUAGE QualifiedDo #-}

module Sierpinski.Triangle ( sierpinskiTriangle ) where

import Art.ContextFree.Definite
import qualified Semigroupoids.Do as S

h :: Float
h = sqrt (1 - 0.5 ** 2.0)

layer :: Int -> SymBuilder
layer 0 = poly [(0.5, -h), (0.5, h)]
layer n
  = let a = layer (n - 1)
        b = a ! Move (0.5, -h)
        c = a ! Move (1, 0)
    in flip (!) (Scale 0.5) $ branch $ S.do
      a
      b
      c

sierpinskiTriangle :: SymBuilder
sierpinskiTriangle = layer 5
