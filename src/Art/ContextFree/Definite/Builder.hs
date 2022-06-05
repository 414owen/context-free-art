{-|
Module      : Art.ContextFree.Definite.Builder
Description : A builder for definite images using semigroupoid analogue of monads
Copyright   : (c) Owen Shepherd, 2019
License     : BSD-3-Clause
Maintainer  : 414owen@gmail.com
Stability   : experimental

You can leverage the `QualifiedDo` extension and `Semigroupoids.Do` to use do notation
to build images.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QualifiedDo #-}

module Art.ContextFree.Definite.Builder
  ( SymBuilder
  , branch
  , modify
  , circle
  , poly
  , reuse
  , runSymBuilder
  , runSymBuilder'
  , (!)
  , (!>)
  , (<!)
  )
  where

import Art.ContextFree.Definite.Grammar
import Art.ContextFree.Geometry
import Art.ContextFree.Modifier

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as M
import Control.Monad.RWS (RWS, runRWS, mapRWS, tell, get, put)
import Data.Functor.Apply (Apply, (<.>))
import Data.Functor.Bind (Bind, (>>-))
import qualified Semigroupoids.Do as S

newtype SymBuilder a = SymBuilder { unSymBuilder :: RWS () (DList SymRef) SymSoup a }
  deriving Functor

instance Apply SymBuilder where
  SymBuilder w <.> SymBuilder w' = SymBuilder $ w <*> w'

instance Bind SymBuilder where
  SymBuilder w >>- f = SymBuilder $ w >>= fmap unSymBuilder f

instance Semigroup a => Semigroup (SymBuilder a) where
  SymBuilder w <> SymBuilder w' = SymBuilder $ do
    a <- w
    b <- w'
    pure $ a <> b

type SymRes a = (a, NonEmpty SymRef, SymSoup)
type SymRes' a = (a, SymRef, SymSoup)

-- | Run a Monadic symbol writer
runSymBuilder :: SymSoup -> SymBuilder a -> SymRes a
runSymBuilder state sb = let (val, state', log) = runRWS (unSymBuilder sb) () state
  in (val, NE.fromList $ DL.toList $ log, state')

addToSoup :: SymSoup -> Symbol SymRef -> (SymSoup, SymRef)
addToSoup state sym = let nk = safeNextKey state in (M.insert nk sym state, SymRef nk)

runSymBuilder' :: SymSoup -> SymBuilder a -> SymRes' a
runSymBuilder' state sb = case runSymBuilder state sb of
  (val, els, state') -> case els of
    x :| [] -> (val, x, state')
    xs -> let (state'', key) = addToSoup state' (Branch xs) in (val, key, state'')

safeNextKey :: IntMap v -> Int
safeNextKey m = case M.null m of
  True -> 0
  False -> fst (M.findMax m) + 1

addAtomic :: Symbol SymRef -> SymBuilder SymRef
addAtomic sym = SymBuilder $ do
  state <- get
  let (state', key) = addToSoup state sym
  put state'
  tell $ pure key
  pure key

data Layer
  = Existing SymRef
  | ManyLayer (NonEmpty (Symbol SymRef), CreateLayer)
  | OneLayer (Symbol SymRef, CreateSingleLayer)
type CreateLayer = NonEmpty SymRef -> Layer
type CreateSingleLayer = SymRef -> Layer

addSymsToSoup :: SymSoup -> NonEmpty (Symbol SymRef) -> (SymSoup, NonEmpty SymRef)
addSymsToSoup soup (x :| xs) = let (soup', SymRef ref) = addToSoup soup x in
  (foldl' (\a e -> fst $ addToSoup a e) soup' xs, SymRef <$> NE.fromList [ref..ref + length xs])

createElem :: CreateLayer -> SymBuilder a -> SymBuilder SymRef
createElem f = SymBuilder . mapRWS (g f) . unSymBuilder
  where
    g :: CreateLayer -> (a, SymSoup, DList SymRef) -> (SymRef, SymSoup, DList SymRef)
    g f' (_, state, w) = case f' $ NE.fromList $ DL.toList w of
      Existing ref -> (ref, state, pure ref)
      OneLayer (sym, h) -> let (state', ref) = addToSoup state sym in
        g (h . NE.head) (ref, state', pure ref)
      ManyLayer (syms, h) -> let (state', refs) = addSymsToSoup state syms in
        g h ((), state', DL.fromList $ NE.toList $ refs)

-- | Monadic analogue of `Branch`
branch :: SymBuilder a -> SymBuilder SymRef
branch = createElem branch'

branch' :: CreateLayer
branch' (x :| []) = Existing x
branch' xs = OneLayer (Branch xs, Existing)

-- | Monadic analogue of `Mod`
modify :: [Modifier] -> SymBuilder a -> SymBuilder SymRef
modify [] = branch
modify mods = createElem f
  where
    f :: CreateLayer
    f (x :| []) = OneLayer (Mod mods x, Existing)
    f xs = OneLayer (Branch xs, \ref -> OneLayer (Mod mods ref, Existing))

class ToModifier m where
  toModifier :: m -> [Modifier]

instance ToModifier Modifier where
  toModifier = pure

instance ToModifier [Modifier] where
  toModifier = id

infixl 2 !
(!) :: ToModifier m => SymBuilder a -> m -> SymBuilder SymRef
(!) s m = modify (toModifier m) s

infixl 1 !>
(!>) :: SymBuilder a -> SymBuilder b -> SymBuilder SymRef
(!>) a b = createElem f $ S.do
  _ <- branch a
  branch b
  where
    f :: CreateLayer
    f (x :| [y]) = OneLayer (Bite x y, Existing)
    -- This is impossible
    f (x :| _) = Existing x

infixl 1 <!
(<!) :: SymBuilder a -> SymBuilder b -> SymBuilder SymRef
(<!) = flip (!>)

-- | Monadic analogue of `Circle`
circle :: Float -> SymBuilder SymRef
circle = addAtomic . Circle

-- | Monadic analogue of `Poly`
poly :: Vec -> NonEmpty Vec -> SymBuilder SymRef
poly v = addAtomic . Poly v

-- | Reuses a symbol
reuse :: SymRef -> SymBuilder SymRef
reuse ref = SymBuilder $ S.do
  tell $ pure ref
  pure ref
