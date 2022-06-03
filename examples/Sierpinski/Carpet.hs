{-# LANGUAGE QualifiedDo #-}

module Sierpinski.Carpet ( sierpinskiCarpet ) where

import Art.ContextFree.Definite
import qualified Semigroupoids.Do as S

third :: Float
third = 1 / 3

square :: SymBuilder
square = poly [(0, 1), (1, 0), (0, -1)]

layer :: Int -> SymBuilder
layer 0 =  square !> modify [Move (third, third), Scale third] square
layer n =
  flip (!) (Scale third) $ branch $ S.do
    row
    row ! Move (0, 2)
    a ! Move (0, 1)
    a ! Move (2, 1)
  where 
    a = layer (n - 1)
    row = branch $ S.do
      a
      a ! Move (1, 0)
      a ! Move (2, 0)

sierpinskiCarpet :: SymBuilder
sierpinskiCarpet = layer 3
