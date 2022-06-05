module Art.ContextFree.Definite.Grammar where

import Art.ContextFree.Modifier
import Art.ContextFree.Geometry (Vec)

import Data.List.NonEmpty (NonEmpty)
import Data.IntMap.Strict (IntMap)

newtype SymRef = SymRef { unSymRef :: Int }
  deriving (Eq, Ord, Show)

type SymSoup = IntMap (Symbol SymRef)
type Image = (SymSoup, SymRef)

-- | A terminal or non-terminal symbol.
data Symbol a

  -- | A non-terminal symbol.
  = Branch (NonEmpty a)
  
  -- | Apply modifications to sub-productions.
  | Mod [Modifier] a

  -- | Produce a circle with a radius.
  | Circle Float

  -- | Remove a chunk from a shape
  | Bite { parent :: a, bite :: a }

  -- | Produce a polygon by relative points.
  --   Starts and ends at (0, 0).
  | Poly Vec (NonEmpty Vec)
  deriving Show
