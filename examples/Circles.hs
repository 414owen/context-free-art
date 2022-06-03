{-# LANGUAGE QualifiedDo #-}

module Circles ( circles ) where

import Art.ContextFree.Definite

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable (foldMap1)

type Layers = Int
type Subdivisions = Int
type LayerMods = [Modifier]

layer :: Subdivisions -> Layers -> LayerMods -> SymBuilder
layer _ 0 _ = circle 1
layer subs layn layerMods =
  circle 1 !>
    flip foldMap1 (1 :| [2..subs]) (\c ->
      modify layerMods sub
        ! Scale r
        ! Move (0, -1 + r)
        ! Rotate (ang * fromIntegral c))
  where
    sub = layer subs (layn - 1) layerMods
    ang = 360.0 / fromIntegral subs
    theta = pi / fromIntegral subs
    r = sin theta / (1 + sin theta)

circles :: Subdivisions -> Layers -> LayerMods -> SymBuilder
circles = layer
