module Sierpinski.Carpet ( sierpinskiCarpet ) where

import Art.ContextFree.Definite
import Data.List.NonEmpty

third :: Float
third = 1.0 / 3

square :: Symbol
square = Poly [(0, 1), (1, 0), (0, -1)]

layer :: Int -> Symbol
layer 0 = Branch $ square
          :| [Mod [Move (third, third), Scale third, Color "#fff"] square]
layer n
  = let a = layer (n - 1)
        row = Branch $ a :| [Mod [Move (1, 0)] a, Mod [Move (2, 0)] a]
    in  Mod [Scale third] $ Branch $ row :|
      [ Mod [Move (0, 2)] row
      , Mod [Move (0, 1)] a
      , Mod [Move (2, 1)] a
      ]

sierpinskiCarpet :: Symbol
sierpinskiCarpet = layer 3
