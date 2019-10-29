module Sierpinski.Triangle ( sierpinskiTriangle ) where

import Art.ContextFree.Definite
import Data.List.NonEmpty

h :: Float
h = sqrt (1 - 0.5 ** 2.0)

layer :: Int -> Symbol
layer 0 = Poly [(0.5, -h), (0.5, h)]
layer n
  = let a = layer (n - 1)
        b = Mod [Move (0.5, -h)] a
        c = Mod [Move (1, 0)] a
    in  Mod [Scale 0.5] $ Branch $ a :| [b, c]

sierpinskiTriangle :: Symbol
sierpinskiTriangle = layer 5
