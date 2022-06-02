{-# LANGUAGE QualifiedDo #-}

module Spiral ( spiral ) where

import Art.ContextFree.Definite

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable (Foldable1(..))
import qualified Semigroupoids.Do as S

move :: SymBuilder a -> SymBuilder ()
move = modify [Move (0, -1.8), Scale 0.8]

armN :: Int -> Float -> SymBuilder ()
armN 0 _   = move $ circle 1
armN n rot = move $ S.do
  circle 1
  modify [Rotate rot] $ armN (n - 1) rot

arm :: SymBuilder ()
arm = armN 11 (-10)

spiral :: NonEmpty Symbol
spiral = runSymBuilder $ S.do
  circle 1
  -- Could also use traverse1
  flip foldMap1 (0 :| [120, 240]) $
    \a -> modify [Rotate a] arm
