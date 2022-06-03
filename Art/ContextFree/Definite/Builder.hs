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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Art.ContextFree.Definite.Builder
  ( SymBuilder
  , branch
  , modify
  , circle
  , poly
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
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Functor.Apply (Apply, (<.>))
import Data.Functor.Bind (Bind, (>>-))

newtype SymWriter a = SymBuilder { unSymBuilder :: Writer (DList Symbol) a }
  deriving Functor

type SymBuilder = SymWriter ()

instance Apply SymWriter where
  SymBuilder w <.> SymBuilder w' = SymBuilder $ w <*> w'

instance Bind SymWriter where
  SymBuilder w >>- f = SymBuilder $ w >>= fmap unSymBuilder f

instance Semigroup a => Semigroup (SymWriter a) where
  SymBuilder w <> SymBuilder w' = SymBuilder $ do
    a <- w
    b <- w'
    pure $ a <> b

-- | Run a Monadic symbol writer
runSymBuilder :: SymBuilder -> NonEmpty Symbol
runSymBuilder = NE.fromList . DL.toList . execWriter . unSymBuilder

runSymBuilder' :: SymBuilder -> Symbol
runSymBuilder' a = case runSymBuilder a of
  x :| [] -> x
  xs -> Branch xs

toWriter :: Symbol -> SymBuilder
toWriter = SymBuilder . tell . pure

-- | Monadic analogue of `Branch`
branch :: SymBuilder -> SymBuilder
branch = toWriter . runSymBuilder'

-- | Monadic analogue of `Mod`
modify :: [Modifier] -> SymBuilder -> SymBuilder
modify [] = id
modify mods = toWriter . Mod mods . runSymBuilder'

infixl 2 !
(!) :: SymBuilder -> Modifier -> SymBuilder
(!) s m = modify [m] s

infixl 1 !>
(!>) :: SymBuilder -> SymBuilder-> SymBuilder
(!>) a b = toWriter $ Bite (runSymBuilder' a) (runSymBuilder' b)

infixl 1 <!
(<!) :: SymBuilder -> SymBuilder-> SymBuilder
(<!) = flip (!>)

-- | Monadic analogue of `Circle`
circle :: Float -> SymBuilder
circle = toWriter . Circle

-- | Monadic analogue of `Poly`
poly :: [Vec] -> SymBuilder
poly = toWriter . Poly
