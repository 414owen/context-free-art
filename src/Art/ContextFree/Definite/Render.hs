{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Art.ContextFree.Definite.Render ( render ) where

import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Foldable
import Data.DList (DList)
import qualified Data.DList as DL
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Linear.Matrix
import Linear.V3
import Text.Blaze
import Text.Blaze.Internal
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Art.ContextFree.Geometry
import Art.ContextFree.Definite.Builder (SymBuilder, runSymBuilder')
import Art.ContextFree.Definite.Grammar
import Art.ContextFree.Modifier

type Bound = (Float, Float, Float, Float)

addBounds :: Bound -> Bound -> Bound
addBounds (x1, y1, x2, y2) (x3, y3, x4, y4) = (min x1 x3, min y1 y3, max x2 x4, max y2 y4)

sequenceBounds :: Foldable f => f Bound -> Bound
sequenceBounds = foldl1 addBounds

{-
-- pos, path
poly :: RenderEnv -> Vec -> NonEmpty Vec -> Res
poly rstate p pts' =
  let 
      pts = p : NE.toList pts'
      (x, y) = displacement rstate
      -- calculate bounds
      (_, b) = foldl nextRes (displacement rstate, (x, y, x, y)) pts
  in  (b, S.path ! A.d (toValue $ toPath $ displacement rstate : pts))
    where
      nextRes ((x, y), b) (dx, dy)
        = let (i, j) = (x + dx, y + dy)
          in  ( (i, j)
              , sequenceBounds [b, (i, j, i, j)]
              )
              -}

{-
-- rad, pos
circle :: Float -> Vec -> Res
circle rad (x, y)
  = ( (x - rad, y - rad, x + rad, y + rad)
    , S.circle
      ! A.r (toValue rad)
      ! A.cx (toValue x)
      ! A.cy (toValue y))
-}

{-
modifyGroup :: Modifier -> Maybe (S.Svg -> S.Svg)
modifyGroup = \case
  Color  c -> Just (! A.fill (toValue c))
  _        -> Nothing

modifyEnv :: Modifier -> RenderM a -> RenderM a
modifyEnv m = local $ \s -> case m of
  Move p   -> s{ displacement = addVecs (displacement s) p }
  _        -> s
  -}

{-
-- TODO optimize with mod mod mod mod
modifySubs :: Modifier -> Symbol SymRef -> Symbol SymRef
modifySubs (Move _)   subs        = subs
modifySubs (Scale s)  (Circle r)  = Circle $ s * r
modifySubs (Scale s)  (Poly v vs) = Poly (scaleVec s v) $ scaleVec s <$> vs
modifySubs (Rotate r) (Poly v vs)   = Poly (rotateZero r v) $ rotateZero r <$> vs
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
-}

{-
joinRes :: Res -> Res -> Res
joinRes (b1, s1) (b2, s2) = (sequenceBounds [b1, b2], s1 >> s2)

sequenceRes :: Traversable t => Res -> t Res -> Res
sequenceRes = foldl joinRes

childLayerContainsBite :: Symbol SymRef -> RenderM Bool
childLayerContainsBite a = case a of
  Branch xs -> do
    xs' <- traverse getSym xs
    or <$> traverse childLayerContainsBite xs'
  Bite _ _ -> pure True
  Mod _ s -> childLayerContainsBite =<< getSym s
  _ -> pure False

incId :: RenderState -> RenderState
incId s = s{ uniqueId = uniqueId s + 1 }

useId :: RenderM String
useId = do
  s <- get
  modify incId
  pure $ "m-" <> show (uniqueId s)

renderSymbol :: Symbol SymRef -> RenderM Res
renderSymbol = undefined
-}

{-\case
  Branch (x :| []) -> renderSymbol x
  Branch (x :| (y : ys)) -> do
    r1 <- renderSymbol x
    rs <- mapM renderSymbol (y : ys)
    pure $ sequenceRes r1 rs
  Circle r -> do
    RenderEnv{displacement} <- ask
    pure $ circle r $ displacement
  Poly p pts -> do
    renv <- ask
    pure $ poly renv p pts
  Bite { parent, bite } -> do
    unid <- useId
    (bounds1, spar) <- renderSymbol parent
    (bounds2, sbite) <- renderSymbol bite
    let bounds@(x, y, x', y') = sequenceBounds [bounds1, bounds2]
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
        -}

getSym' :: SymRef -> SymSoup -> Symbol SymRef
getSym' ref s = s M.! unSymRef ref

{-
getSym :: SymRef -> RenderM (Symbol SymRef)
getSym ref = do
  m <- soup <$> ask
  return $ getSym' ref m
-}

data Modifiers
  = Modifiers
  { transform :: M33 Float
  , scale :: Float
  }

instance Semigroup Modifiers where
  m1 <> m2 = Modifiers (transform m1 !*! transform m2) (scale m1 * scale m2)

toRad :: Float -> Float
toRad = (* pi) . (/ 180)

rotateMat :: Float -> M33 Float
rotateMat r =
  let
    r' = toRad r
    c = cos r'
    s = sin r'
  in V3
    (V3 c (0 - s) 0)
    (V3 s c 0)
    (V3 0 0 1)

translateMat :: Vec -> M33 Float
translateMat (x, y) = V3
  (V3 1 0 x)
  (V3 0 1 y)
  (V3 0 0 1)

-- uniform x/y scale
scaleMat :: Float -> M33 Float
scaleMat s = V3
  (V3 s 0 0)
  (V3 0 s 0)
  (V3 0 0 1)

addMod :: Modifier -> Modifiers -> Modifiers
addMod mod mods = case mod of
  Scale n -> mods <> Modifiers (scaleMat n) n
  Rotate n -> mods <> Modifiers (rotateMat n) 1
  Move n -> mods <> Modifiers (translateMat n) 1
  _ -> mods

addMods :: Foldable f => f Modifier -> Modifiers -> Modifiers
addMods ms m = foldl' (flip addMod) m ms

noMods :: Modifiers
noMods = Modifiers identity 1

traverseBounds :: (Functor f, Foldable f) => (a -> Bound) -> f a -> Bound
traverseBounds f = sequenceBounds . fmap f

ptToBound :: Vec -> Bound
ptToBound (x, y) = (x, y, x, y)

transformPt :: M33 Float -> Vec -> Vec
transformPt mat (x, y) = case mat !* V3 x y 1 of
  V3 x' y' _ -> (x', y')

type BoundM = State (IntMap Bound)

getBounds :: SymSoup -> SymRef -> Bound
getBounds symsoup root = evalState (f noMods root) M.empty
  where
    f :: Modifiers -> SymRef -> BoundM Bound
    f mods@(Modifiers mat scale) ref = do
      let sym = getSym' ref symsoup
      cache <- get
      case M.lookup (unSymRef ref) cache of
        Just bound -> pure bound
        Nothing -> case sym of
          Branch xs -> sequenceBounds <$> traverse (f mods) xs
          Circle r -> let (x, y) = transformPt mat (0, 0)
                          r' = max 0 $ scale * r
                      in
            pure (x - r', y - r', x + r', y + r')
          Poly p (p' :| ps) -> pure $ traverseBounds (ptToBound . transformPt mat) $ p : p' : ps
          Bite { parent, bite } -> do
            bounds1 <- f mods parent
            bounds2 <- f mods bite
            pure $ addBounds bounds1 bounds2
          Mod ms sub -> f (addMods ms mods) sub

fourTupLst :: (a, a, a, a) -> [a]
fourTupLst (a, b, c, d) = [a, b, c, d]

toSVG :: Bound -> S.Svg -> S.Svg
toSVG bound
  = S.docTypeSvg
  ! A.version "1.1"
  ! A.viewbox (toValue $ unwords $ show <$> fourTupLst bound)

boundsToViewBox :: Bound -> Bound
boundsToViewBox (x1, y1, x2, y2) = (x1, y1, x2 - x1, y2 - y1)

type CollectM = Writer (DList SymRef)

collectRefs :: SymSoup -> SymRef -> DList SymRef
collectRefs soup ref = execWriter (count ref)
  where
    -- TODO cache results in an IntMap state, instead of using MonadWriter
    count :: SymRef -> CollectM ()
    count ref' = do
      tell $ pure ref'
      let sym = getSym' ref' soup
      case sym of
        Branch xs -> traverse_ count $ NE.toList xs
        Mod _ x -> count x
        Bite a b -> count a >> count b
        _ -> pure ()

countUnion :: IntMap Int -> IntMap Int -> IntMap Int
countUnion = M.unionWith (+)

toSingleCount :: Int -> IntMap Int
toSingleCount a = M.singleton a 1

type Id = Int

type RenderM = RWS () S.Svg IntSet

toSingleEl :: NonEmpty S.Svg -> S.Svg
toSingleEl (x :| []) = x
toSingleEl xs = S.g $ fold xs

modifierAttrs :: [Modifier] -> [Attribute]
modifierAttrs ms = cols <> transforms
  -- A.transform . toValue . unwords . catMaybes . fmap f
  where
    toCol :: Modifier -> Maybe Attribute
    toCol m = case m of
      Color s -> Just $ A.fill $ toValue s
      _ -> Nothing

    cols :: [Attribute]
    cols = case reverse $ catMaybes $ toCol <$> ms of
      (x : _) -> [x]
      [] -> []

    transforms :: [Attribute]
    transforms = case catMaybes $ toTransform <$> ms of
      [] -> []
      xs -> [A.transform $ toValue $ unwords xs]
  
    toTransform :: Modifier -> Maybe String
    toTransform m = case m of
      Scale n -> Just $ "scale(" <> show n <> ")"
      Move (x, y) -> Just $ "translate(" <> show x <> " " <> show y <> ")"
      Rotate r -> Just $ "rotate(" <> show r <> ")"
      _ -> Nothing

applyAttrs :: Attributable h => h -> [Attribute] -> h
applyAttrs = foldl' (!)

-- | Create a drawing from a grammar.
--   In order to get a string representation, you'll need to use one of
--   blaze-svg's render functions, for example 'renderSvg'.
render' :: Image -> S.Svg
render' (soup, root) = finalise els
  where
    allRefs :: [SymRef]
    allRefs = DL.toList $ collectRefs soup root

    useCounts :: IntMap Int
    useCounts = foldl countUnion M.empty $ toSingleCount . unSymRef <$> allRefs
    
    reusedRefs :: IntMap Id
    reusedRefs = M.fromList $ (`zip` [0..]) $ fmap fst $ filter ((> 1) . snd) $ M.assocs useCounts

    bounds :: Bound
    bounds = getBounds soup root

    els :: NonEmpty S.Svg
    els = let (e :| es, w) = evalRWS (renderEl root) () mempty in
      e :| (S.defs w : es)

    finalise :: Foldable t => t S.Svg -> S.Svg
    finalise els = toSVG (boundsToViewBox bounds) $ fold els

    toId :: SymRef -> String
    toId n = "r" <> show (reusedRefs M.! unSymRef n)

    toLink :: SymRef -> Attribute
    toLink ref = A.xlinkHref $ toValue $ "url(#" <> toId ref <> ")"

    renderEl :: SymRef -> RenderM (NonEmpty S.Svg)
    renderEl ref = do
      let sym = getSym' ref soup
      s <- get
      modify $ IS.insert $ unSymRef ref
      if not $ IS.member (unSymRef ref) s
      then pure $ pure $ S.use ! toLink ref
      else case sym of
        Branch xs -> fmap fold <$> traverse renderEl xs
        Circle r -> pure $ pure $ S.circle S.! A.r (toValue r)
        Poly p (p' :| ps) ->
          pure $ pure $ S.path ! A.d (toValue $ unwords $ ("l" <>) . show <$> p : p' : ps)
        Bite { parent, bite } -> do
          parentEl <- toSingleEl <$> renderEl parent
          biteEls <- renderEl bite
          tell $ S.mask ! A.id_ (toValue $ toId ref) $ fold biteEls
          pure $ pure $ parentEl ! toLink bite
        Mod ms sub -> do
          subs <- renderEl sub
          pure $ pure $ applyAttrs (toSingleEl subs) $ modifierAttrs ms

class Renderable a where
  render :: a -> S.Svg

instance Renderable Image where
  render = render'

instance Renderable (SymBuilder SymRef) where
  render b = let (_, root, soup) = runSymBuilder' b in
    render (soup, root)