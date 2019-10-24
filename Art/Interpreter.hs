{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Art.Interpreter ( interpret ) where

import Control.Arrow
import Data.List
import Data.List.NonEmpty hiding (reverse)
import Data.Maybe
import System.Random
import Text.Blaze
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Art.Geometry
import Art.Grammar
import Art.Util

type Bound = (Float, Float, Float, Float)
type BoundRes = Maybe Bound
type Res = (BoundRes, S.Svg)
type State = Vec

emptyRes :: Res
emptyRes = (Nothing, mempty)

combineBounds :: [BoundRes] -> BoundRes
combineBounds boundsM =
  let bounds = catMaybes boundsM
      (x1, y1, x2, y2) = unzip4 bounds
  in if null bounds then Nothing else
    Just (minimum x1, minimum y1, maximum x2, maximum y2)

-- pos, path
poly :: State -> [Vec] -> Res
poly pos pts =
  let newPts = pos : pts
      (x, y) = pos
      (_, b) = foldl nextRes (pos, Just (x, y, x, y)) newPts
  in  (b, S.path ! A.d (toValue $ toPath newPts))
    where
      nextRes ((x, y), b) (dx, dy)
        = let (i, j) = (x + dx, y + dy)
          in  ( (i, j)
              , combineBounds [b, Just (i, j, i, j)]
              )

-- rad, pos
circle :: Float -> Vec -> Res
circle rad (x, y)
  = ( Just (x - rad, y - rad, x + rad, y + rad)
    , S.circle
      ! A.r (toValue rad)
      ! A.cx (toValue x)
      ! A.cy (toValue y))

modifyGroup :: Modifier -> Maybe (S.Svg -> S.Svg)
modifyGroup = \case
    Color  c -> Just (! A.fill (toValue c))
    _        -> Nothing

modifyState :: State -> Modifier -> State
modifyState pos = \case
  Move p   -> addVecs pos p
  _        -> pos

modifySubs :: Modifier -> Symbol -> Symbol
modifySubs (Move _)   subs        = subs
modifySubs (Scale s)  (Circle r)  = Circle $ s * r
modifySubs (Scale s)  (Poly vs)   = Poly $ scaleVec s <$> vs
modifySubs (Rotate r) (Poly vs)   = Poly $ rotateZero r <$> vs
modifySubs m          (NonTerminal prods)
    = NonTerminal $ second (modifySubs m) <$> prods
modifySubs mo (Mod ms a)
  = Mod (modifyMod mo <$> ms) $ modifySubs mo a
  where
    modifyMod (Scale  s) (Move m) = Move $ scaleVec  s m
    modifyMod (Rotate r) (Move m) = Move $ rotateZero r m
    modifyMod _          m        = m
modifySubs _ subs = subs

in100 :: Int -> Int
in100 = (`mod` 100) . abs

joinRes :: Res -> Res -> Res
joinRes (b1, s1) (b2, s2) = (combineBounds [b1, b2], s1 >> s2)

sequenceRes :: (Monad m, Traversable t) => t (m Res) -> m Res
sequenceRes rs = foldl joinRes emptyRes <$> sequence rs

interpretNonTerminal :: State -> Production -> IO Res
interpretNonTerminal state (prob, sym)
  = (< prob) . fromIntegral . in100 <$> randomIO
    >>= \case
      True -> interpretSymbol state sym
      False -> pure emptyRes

interpretSymbol :: State -> Symbol -> IO Res
interpretSymbol state = \case
  NonTerminal (x :| []) -> interpretNonTerminal state x
  NonTerminal (x :| (y: ys)) ->
    sequenceRes (interpretNonTerminal state <$> (x :| y : ys))
  Circle r -> pure $ circle r state
  Poly pts -> pure $ poly state pts
  Mod [] sym -> interpretSymbol state sym
  Mod ms sym ->
    let groupMods = catMaybes $ modifyGroup <$> ms
        ed = if null groupMods then id else foldl (flip fmap) S.g groupMods
        sub = interpretMods state ms sym
    in  second ed <$> sub
  where
    interpretMods state' [] sym       = interpretSymbol state' sym
    interpretMods state' (m : ms) sym =
      let newState = modifyState state' m
          newMods  = modifySubs m $ Mod ms sym
      in  interpretSymbol newState newMods

fourTupLst :: (a, a, a, a) -> [a]
fourTupLst (a, b, c, d) = [a, b, c, d]

toSVG :: Bound -> S.Svg -> S.Svg
toSVG bound
  = S.docTypeSvg
  ! A.version "1.1"
  ! A.viewbox (toValue $ unwords $ show <$> fourTupLst bound)

boundsToViewBox :: Bound -> Bound
boundsToViewBox (x1, y1, x2, y2) = (x1, y1, x2 - x1, y2 - y1)

-- | Create a drawing from a grammar.
--   In order to get a string representation, you'll need to use one of
--   blaze-svg's render functions, for example 'renderSvg'.
interpret :: Symbol -> IO S.Svg
interpret sym =
  finalise <$> interpretSymbol (0, 0) sym
    where
      finalise :: Res -> S.Svg
      finalise (bounds, svg) = toSVG (boundsToViewBox (fromMaybe (0, 0, 0, 0) bounds)) svg
