{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Art.ContextFree.Definite.Render ( Render(..) ) where

import Control.Monad.RWS
import Data.Foldable
import Data.List
import Data.DList (DList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Text.Blaze
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Art.ContextFree.Geometry
import Art.ContextFree.Definite.Grammar
import Art.ContextFree.Definite.Builder (SymBuilder, runSymBuilder)
import Art.ContextFree.Modifier
import Art.ContextFree.Util

type Bound = (Float, Float, Float, Float)
type Res = (Bound, S.Svg)
newtype RenderEnv = RenderEnv { displacement :: Vec }
newtype RenderState = RenderState { uniqueId :: Int }
type RenderM = RWS RenderEnv (DList S.Svg) RenderState

combineBounds :: [Bound] -> Bound
combineBounds bounds =
  let (x1, y1, x2, y2) = unzip4 bounds
  in  (minimum x1, minimum y1, maximum x2, maximum y2)

-- pos, path
poly :: RenderEnv -> [Vec] -> Res
poly rstate pts =
  let (x, y) = displacement rstate
      --calculate bounds
      (_, b) = foldl nextRes (displacement rstate, (x, y, x, y)) pts
  in  (b, S.path ! A.d (toValue $ toPath $ displacement rstate : pts))
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

modifyEnv :: Modifier -> RenderM a -> RenderM a
modifyEnv m = local $ \s -> case m of
  Move p   -> s{ displacement = addVecs (displacement s) p }
  _        -> s

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
modifySubs m (Bite a b) = Bite (modifySubs m a) (modifySubs m b)
modifySubs _ subs = subs

joinRes :: Res -> Res -> Res
joinRes (b1, s1) (b2, s2) = (combineBounds [b1, b2], s1 >> s2)

sequenceRes :: Traversable t => Res -> t Res -> Res
sequenceRes = foldl joinRes

childLayerContainsBite :: Symbol -> Bool
childLayerContainsBite a = case a of
  Branch xs -> any childLayerContainsBite xs
  Bite _ _ -> True
  Mod _ s -> childLayerContainsBite s
  _ -> False

incId :: RenderState -> RenderState
incId s = s{ uniqueId = uniqueId s + 1 }

useId :: RenderM String
useId = do
  s <- get
  modify incId
  pure $ "m-" <> show (uniqueId s)

renderSymbol :: Symbol -> RenderM Res
renderSymbol = \case
  Branch (x :| []) -> renderSymbol x
  Branch (x :| (y : ys)) -> do
    r1 <- renderSymbol x
    rs <- mapM renderSymbol (y : ys)
    pure $ sequenceRes r1 rs
  Circle r -> do
    RenderEnv{displacement} <- ask
    pure $ circle r $ displacement
  Poly pts -> do
    renv <- ask
    pure $ poly renv pts
  Bite { parent, bite } -> do
    unid <- useId
    (bounds1, spar) <- renderSymbol parent
    (bounds2, sbite) <- renderSymbol bite
    let bounds@(x, y, x', y') = combineBounds [bounds1, bounds2]
    let spar' = if childLayerContainsBite parent then S.g spar else spar
    tell $ pure $ S.mask ! A.id_ (stringValue unid) $ do
      S.rect
        ! A.x (toValue x)
        ! A.y (toValue y)
        ! A.width (toValue $ x' - x)
        ! A.height (toValue $ y' - y)
        ! A.fill "#fff"
      sbite
    let s' = spar' ! A.mask (stringValue $ "url(#" <> unid <> ")")
    pure $ (bounds, s')
  Mod [] sym -> renderSymbol sym
  Mod ms sym -> do
    let groupMods = catMaybes $ modifyGroup <$> ms
    let ed = if null groupMods then id else foldl (flip fmap) S.g groupMods
    sub <- renderMods ms
    pure $ ed <$> sub

    where
      renderMods [] = renderSymbol sym
      renderMods (m : ms') = modifyEnv m $ do
        let newMods = modifySubs m $ Mod ms' sym
        renderSymbol newMods

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
    finalise $ evalRWS (renderSymbol sym) (RenderEnv (0, 0)) (RenderState 0)
      where
        finalise :: (Res, DList S.Svg) -> S.Svg
        finalise ((bounds, svg), els) = toSVG (boundsToViewBox bounds) $ do
          svg
          fold els

instance Render (NonEmpty Symbol) where
  render = render . Branch

instance Render SymBuilder where
  render = render . runSymBuilder
