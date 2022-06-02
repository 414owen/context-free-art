{-# LANGUAGE QualifiedDo #-}

module Spiral ( spiral ) where

import Art.ContextFree.Definite

import Data.List.NonEmpty (NonEmpty)
import qualified Semigroupoids.Do as S

move :: SymWriter a -> SymWriter ()
move = modify [Move (0, -1.8), Scale 0.8]

armN :: Int -> Float -> SymWriter ()
armN 0 _   = move $ circle 1
armN n rot = move $ S.do
  circle 1
  modify [Rotate rot] $ armN (n - 1) rot

arm :: SymWriter ()
arm = armN 11 (-10)

spiral :: NonEmpty Symbol
spiral = runSymWriter $ S.do
  circle 1
  arm
  modify [Rotate 120] arm
  modify [Rotate 240] arm
