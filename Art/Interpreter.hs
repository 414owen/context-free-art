{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Art.Interpreter ( interpret ) where

import TextShow
import Data.List
import Data.List.NonEmpty hiding (reverse)
import Data.Tuple.Extra
import Data.Functor
import Data.Function
import Data.Maybe
import System.Random
import Text.Blaze
import qualified Data.Text as T
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Art.Geometry
import Art.Grammar
import Art.Util

type Bound = (Float, Float, Float, Float)
type BoundRes = Maybe Bound
type Res = (BoundRes, S.Svg)

data State
  = State
    { position :: Vec
    , scale :: Float
    }

emptyBound = Nothing
emptyRes = (emptyBound, mempty)
zeroPt = (0, 0)
emptyState = State { position = zeroPt, scale = 1.0 }

zero :: AttributeValue
zero = toValue (0 :: Int)

combineBounds :: [BoundRes] -> BoundRes
combineBounds boundsM =
  let bounds = catMaybes boundsM
      (x1, y1, x2, y2) = unzip4 bounds
  in if null bounds then Nothing else
    Just (minimum x1, minimum y1, maximum x2, maximum y2)

-- pos, path
poly :: State -> [Vec] -> Res
poly State{ position=pos, scale=scale } pts = 
  let newPts = scaleVec scale <$> pos : pts
      (x, y) = pos
      (_, b) = foldl nextRes (pos, Just (x, y, x, y)) newPts
  in  (b, S.path ! A.d (toValue $ toPath newPts))
    where
      nextRes ((x, y), b) (dx, dy)
        = let (i, j) = (x + dx, y + dy)
          in ( (i, j)
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

groupModifier :: Modifier -> Maybe (S.Svg -> S.Svg)
groupModifier = \case
    Color  c -> Just (! A.fill (toValue c))
    _        -> Nothing

modifyState :: State -> Modifier -> State
modifyState s@State{ position = pos, scale = scale } = \case
  Move p  -> s{ position = addVecs pos (scaleVec scale p) }
  Scale n -> s{ scale = scale * n }
  _       -> s

in100 :: Int -> Int
in100 = (`mod` 100) . abs

foldMods :: State -> [Modifier] -> (State, S.Svg -> S.Svg)
foldMods state mods =
  let newState = foldl modifyState state mods
      groupMods = mapMaybe groupModifier mods
      maybeLayer =
        if null groupMods
        then id
        else foldl (<&>) S.g groupMods
  in  (newState, maybeLayer)

joinRes :: Res -> Res -> Res
joinRes (b1, s1) (b2, s2) = (combineBounds [b1, b2], s1 >> s2)

sequenceRes :: (Monad m, Traversable t) => t (m Res) -> m Res
sequenceRes rs = foldl joinRes emptyRes <$> sequence rs

interpretNonTerminal :: State -> Production -> IO Res
interpretNonTerminal state prod@(prob, sym)
  = (< prob) . fromIntegral . in100 <$> randomIO
    >>= \case
      True -> interpretSymbol state sym
      False -> pure emptyRes

interpretSymbol :: State -> Symbol -> IO Res
interpretSymbol state@State{ position = pos, scale = scale }
  = \case
    NonTerminal (x :| []) -> interpretNonTerminal state x
    NonTerminal (x :| (y: ys)) ->
      sequenceRes (interpretNonTerminal state <$> (x :| y : ys))
    Circle r -> pure $ circle (r * scale) pos
    Poly pts -> pure $ poly state pts
    Mod mods sym ->
        let (newState, layerMod) = foldMods state mods
        in second layerMod <$> interpretSymbol newState sym

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
  finalise <$> interpretSymbol emptyState sym
    where
      finalise :: Res -> S.Svg
      finalise (Just bounds, svg) = toSVG (boundsToViewBox bounds) svg
