module Art.Grammar where

import Data.List.NonEmpty
import Art.Geometry

-- | Change the style applied to all downstream terminal symbols.
data Modifier
  = Color String
  | Scale Float
  | Move Vec

-- | A production rule, including a starting probability of generation,
--   a list of styles to be applied to sub-grammars, and a non-empty list of
--   symbols to produce.
type Production = (Float, Symbol)

-- | A terminal or non-terminal symbol.
data Symbol

  -- | A non-terminal symbol.
  = NonTerminal (NonEmpty Production)

  -- | Apply modifications to sub-productions.
  | Mod [Modifier] Symbol

  -- | Produce a circle with a radius.
  | Circle Float

  -- | Produce a polygon by relative points.
  --   Starts and ends at (0, 0).
  | Poly [Vec]
