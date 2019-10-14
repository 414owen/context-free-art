module Geometry where

import Control.Arrow
import Data.Tuple.Extra
import Data.Biapplicative

type Vec = (Float, Float)

addVecs :: Vec -> Vec -> Vec
addVecs = biliftA2 (+) (+)

reflectVec :: Vec -> Vec
reflectVec = both negate

subVecs :: Vec -> Vec -> Vec
subVecs v1 v2 = addVecs v1 $ reflectVec v2

scaleVec :: Float -> Vec -> Vec
scaleVec n = both (* n)
