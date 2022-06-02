{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Art.ContextFree.Definite.Render ( Render(..) ) where

import Data.List
import Data.List.NonEmpty hiding (reverse)
import Data.Maybe
import Text.Blaze
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Art.ContextFree.Geometry
import Art.ContextFree.Definite.Grammar
import Art.ContextFree.Definite.Builder (SymBuilder, runSymBuilder)
import Art.ContextFree.Modifier
import Art.ContextFree.Util

type Bound = (Float, Float, Float, Float)
type Res = (Bound, S.Svg)
type State = Vec

combineBounds :: [Bound] -> Bound
combineBounds bounds =
  let (x1, y1, x2, y2) = unzip4 bounds
  in  (minimum x1, minimum y1, maximum x2, maximum y2)

-- pos, path
poly :: State -> [Vec] -> Res
poly pos pts =
  let (x, y) = pos
      --calculate bounds
      (_, b) = foldl nextRes (pos, (x, y, x, y)) pts
  in  (b, S.path ! A.d (toValue $ toPath $ pos : pts))
    where
      nextRes ((x, y), b) (dx, dy)
        = let (i, j) = (x + dx, y + dy)
          in  ( (i, j)
              , combineBounds [b, (i, j, i, j)]
              )

-- rad, pos
circle :: Float -> Vec -> Res
circle rad (x, y)
  = ( (x - rad, y - rad, x + rad, y + rad)
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
modifySubs m          (Branch prods)
    = Branch $ modifySubs m <$> prods
modifySubs mo (Mod ms a)
  = Mod (modifyMod mo <$> ms) $ modifySubs mo a
  where
    modifyMod (Scale  s) (Move m) = Move $ scaleVec  s m
    modifyMod (Rotate r) (Move m) = Move $ rotateZero r m
    modifyMod _          m        = m
modifySubs _ subs = subs

joinRes :: Res -> Res -> Res
joinRes (b1, s1) (b2, s2) = (combineBounds [b1, b2], s1 >> s2)

sequenceRes :: Traversable t => Res -> t Res -> Res
sequenceRes = foldl joinRes

renderSymbol :: State -> Symbol -> Res
renderSymbol state = \case
  Branch (x :| []) -> renderSymbol state x
  Branch (x :| (y: ys)) ->
    sequenceRes (renderSymbol state x) (renderSymbol state <$> (y : ys))
  Circle r -> circle r state
  Poly pts -> poly state pts
  Mod [] sym -> renderSymbol state sym
  Mod ms sym ->
    let groupMods = catMaybes $ modifyGroup <$> ms
        ed = if null groupMods then id else foldl (flip fmap) S.g groupMods
        sub = renderMods state ms sym
    in  ed <$> sub
  where
    renderMods state' [] sym       = renderSymbol state' sym
    renderMods state' (m : ms) sym =
      let newState = modifyState state' m
          newMods  = modifySubs m $ Mod ms sym
      in  renderSymbol newState newMods

fourTupLst :: (a, a, a, a) -> [a]
fourTupLst (a, b, c, d) = [a, b, c, d]

toSVG :: Bound -> S.Svg -> S.Svg
toSVG bound
  = S.docTypeSvg
  ! A.version "1.1"
  ! A.viewbox (toValue $ unwords $ show <$> fourTupLst bound)

boundsToViewBox :: Bound -> Bound
boundsToViewBox (x1, y1, x2, y2) = (x1, y1, x2 - x1, y2 - y1)

class Render a where
  -- | Create a drawing from a grammar.
  --   In order to get a string representation, you'll need to use one of
  --   blaze-svg's render functions, for example 'renderSvg'.
  render :: a -> S.Svg

instance Render Symbol where
  render sym =
    finalise $ renderSymbol (0, 0) sym
      where
        finalise :: Res -> S.Svg
        finalise (bounds, svg) = toSVG (boundsToViewBox bounds) svg

instance Render (NonEmpty Symbol) where
  render = render . Branch

instance Render SymBuilder where
  render = render . runSymBuilder
