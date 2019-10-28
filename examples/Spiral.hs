module Spiral ( spiral ) where

import Art.ContextFree.Definite
import Data.List.NonEmpty

move :: Symbol -> Symbol
move = Mod [Move (0, -1.8), Scale 0.8]

armN :: Int -> Float -> Symbol
armN 0 _   = move $ Circle 1
armN n rot = move $ Branch $ Circle 1 :| [Mod [Rotate rot] $ armN (n - 1) rot]

arm :: Symbol
arm = armN 11 (-10)

spiral :: Symbol
spiral = Branch $ Circle 1 :|
  [ arm
  , Mod [Rotate 120] arm
  , Mod [Rotate 240] arm
  ]
