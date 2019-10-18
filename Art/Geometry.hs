module Art.Geometry where

import Control.Arrow
import Data.Biapplicative

-- | A vector in 2d euclidian space.
type Vec = (Float, Float)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

addVecs :: Vec -> Vec -> Vec
addVecs = biliftA2 (+) (+)

reflectVec :: Vec -> Vec
reflectVec = both negate

subVecs :: Vec -> Vec -> Vec
subVecs v1 v2 = addVecs v1 $ reflectVec v2

scaleVec :: Float -> Vec -> Vec
scaleVec n = both (* n)
