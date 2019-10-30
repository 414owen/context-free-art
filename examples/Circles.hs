module Circles ( circles ) where

import Art.ContextFree.Definite
import Data.List.NonEmpty

type Layers = Int
type Subdivisions = Int
type LayerMods = [Modifier]

withCol :: Bool -> Symbol -> Symbol
withCol b = Mod [Color $ if b then "#000" else "#fff"]

layer :: Bool -> Subdivisions -> Layers -> LayerMods -> Symbol
layer isDark _    0    _= withCol isDark $ Circle 1
layer isDark subs layn layerMods
  = let a = layer (not isDark) subs (layn - 1) layerMods
        ang = 360.0 / fromIntegral subs
        theta = pi / fromIntegral subs
        r = sin theta / (1 + sin theta)
    in  withCol isDark $
      Branch $ Circle 1 :|
        fmap (\c -> Mod
          ([ Rotate $ ang * fromIntegral c
          , Move (0, -1 + r)
          , Scale r
          ] <> layerMods) a) [1..subs]

circles :: Subdivisions -> Layers -> LayerMods -> Symbol
circles = layer True
