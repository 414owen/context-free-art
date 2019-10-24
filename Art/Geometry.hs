module Art.Geometry where

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

radDegRatio :: Float
radDegRatio = pi / 180

degToRad :: Float -> Float
degToRad = (* radDegRatio)

rotateZero :: Float -> Vec -> Vec
rotateZero amtd (x, y)
  = (x * cos amt - y * sin amt, y * cos amt + x * sin amt)
    where
      amt = degToRad amtd
