{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List
import Data.List.NonEmpty
import Data.Tuple.Extra
import Data.Functor
import Data.Function
import Data.Maybe
import System.Random
import Text.Blaze
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Geometry
import Grammar

type Bound = (Float, Float, Float, Float)
type BoundRes = Maybe Bound
type Res = (BoundRes, S.Svg)

data State
  = State
    { position :: Vec
    , scale :: Float
    }

emptyState = State { position = (0.0, 0.0), scale = 1.0 }

notimpl = error "not implemented"

zero :: AttributeValue
zero = toValue (0 :: Int)

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
    Color  c -> Just (! A.color  (toValue c))
    Stroke n -> Just (! A.stroke (toValue n))
    _        -> Nothing

modifyState :: State -> Modifier -> State
modifyState s (Move pos) = s { position = addVecs (position s) pos }

in100 :: Int -> Int
in100 = (`mod` 100) . abs

combineBounds :: [BoundRes] -> BoundRes
combineBounds boundsM =
  let bounds = catMaybes boundsM
      (x1, y1, x2, y2) = unzip4 bounds
  in if null bounds then Nothing else
    Just (minimum x1, minimum y1, maximum x2, maximum y2)

foldMods :: State -> [Modifier] -> (State, S.Svg -> S.Svg)
foldMods state mods =
  let newState = foldl modifyState state mods
      groupMods = mapMaybe groupModifier mods
      maybeLayer =
        if null groupMods
        then id
        else foldl (<&>) S.g groupMods
  in  (newState, maybeLayer)

emptyBound = Nothing
emptyRes = (emptyBound, mempty)

joinRes :: Res -> Res -> Res
joinRes (b1, s1) (b2, s2) = (combineBounds [b1, b2], s1 >> s2)

sequenceRes :: (Monad m, Traversable t) => t (m Res) -> m Res
sequenceRes rs = foldl joinRes emptyRes <$> sequence rs

interpretNonTerminal :: State -> Production -> IO Res
interpretNonTerminal state prod@(prob, mods, syms)
  = (< prob) . in100 <$> randomIO
    >>= \case
      True ->
        let (newState, layerMod) = foldMods state mods
        in second layerMod <$> sequenceRes (interpret newState <$> syms)
      False -> pure emptyRes

interpret :: State -> Symbol -> IO Res
interpret state@State{ position = pos, scale = scale }
  = \case
    NonTerminal (x :| []) -> interpretNonTerminal state x
    NonTerminal (x :| (y: ys)) ->
      sequenceRes (interpretNonTerminal state <$> (x :| y : ys))
    Circle r -> pure $ circle (r * scale) pos
    Poly _ -> pure (emptyBound, notimpl)
    Path _ -> pure (emptyBound, notimpl)

fourTupLst :: (a, a, a, a) -> [a]
fourTupLst (a, b, c, d) = [a, b, c, d]

toSVG :: Bound -> S.Svg -> S.Svg
toSVG bound
  = S.docTypeSvg
  ! A.version "1.1"
  ! A.viewbox (toValue $ unwords $ show <$> fourTupLst bound)

boundsToViewBox :: Bound -> Bound
boundsToViewBox (x1, y1, x2, y2) = (x1, y1, x2 - x1, y2 - y1)

testGrammar :: Symbol
testGrammar = NonTerminal ((85, [Move (2, 0)], testGrammar :| [Circle 1]) :| [])

main :: IO ()
main = do
  (Just bounds, svg) <- interpret emptyState testGrammar
  putStrLn $ renderSvg $ toSVG (boundsToViewBox bounds) svg
