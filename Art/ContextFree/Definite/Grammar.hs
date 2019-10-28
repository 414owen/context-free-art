module Art.ContextFree.Definite.Grammar where

import Data.List.NonEmpty
import Art.ContextFree.Modifier
import Art.ContextFree.Geometry (Vec)

-- | A terminal or non-terminal symbol.
data Symbol

  -- | A non-terminal symbol.
  = Branch (NonEmpty Symbol)

  -- | Apply modifications to sub-productions.
  | Mod [Modifier] Symbol

  -- | Produce a circle with a radius.
  | Circle Float

  -- | Produce a polygon by relative points.
  --   Starts and ends at (0, 0).
  | Poly [Vec]
