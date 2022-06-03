{-# LANGUAGE QualifiedDo #-}

module Circles ( circles ) where

import Art.ContextFree.Definite

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable (foldMap1)
import qualified Semigroupoids.Do as S

type Layers = Int
type Subdivisions = Int
type LayerMods = [Modifier]

withCol :: Bool -> SymBuilder -> SymBuilder
withCol b = modify [Color $ if b then "#000" else "#fff"]

layer :: Bool -> Subdivisions -> Layers -> LayerMods -> SymBuilder
layer isDark _    0    _ = withCol isDark $ circle 1
layer isDark subs layn layerMods =
  withCol isDark $ S.do
    circle 1
    flip foldMap1 (1 :| [2..subs]) $ \c ->
      modify layerMods a
        ! Scale r
        ! Move (0, -1 + r)
        ! Rotate (ang * fromIntegral c)
  where
    a = layer (not isDark) subs (layn - 1) layerMods
    ang = 360.0 / fromIntegral subs
    theta = pi / fromIntegral subs
    r = sin theta / (1 + sin theta)

circles :: Subdivisions -> Layers -> LayerMods -> SymBuilder
circles = layer True
