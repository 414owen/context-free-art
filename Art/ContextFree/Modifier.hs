module Art.ContextFree.Modifier where

import Art.ContextFree.Geometry (Vec)

-- | Change the style applied to all downstream terminal symbols.
data Modifier
  = Color String
  | Scale Float
  | Move Vec
  | Rotate Float
