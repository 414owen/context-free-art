module Sierpinski ( sierpinski ) where

import Art.ContextFree.Definite
import Data.List.NonEmpty

layer :: Int -> Symbol
layer 0 = Circle 1
layer n
  = let a = Mod [Scale 0.5] $ layer (n - 1)
        b = Mod [Rotate 30, Move (0, -1), Rotate (-30)] a
        c = Mod [Move (1, 0)] a
    in  Branch $ a :| [b, c]

sierpinski :: Symbol
sierpinski = layer 5
