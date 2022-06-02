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
  )
  where

import Art.ContextFree.Definite.Grammar
import Art.ContextFree.Geometry
import Art.ContextFree.Modifier

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.DList (DList)
import qualified Data.DList as DL
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Functor.Apply (Apply, (<.>))
import Data.Functor.Bind (Bind, (>>-))

newtype SymBuilder a = SymBuilder { unSymBuilder :: Writer (DList Symbol) a }
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

-- | Run a Monadic symbol writer
runSymBuilder :: SymBuilder a -> NonEmpty Symbol
runSymBuilder = NE.fromList . DL.toList . execWriter . unSymBuilder

toWriter :: Symbol -> SymBuilder ()
toWriter = SymBuilder . tell . pure

-- | Monadic analogue of `Branch`
branch :: SymBuilder a -> SymBuilder ()
branch = toWriter . Branch . runSymBuilder

-- | Monadic analogue of `Mod`
modify :: [Modifier] -> SymBuilder a -> SymBuilder ()
modify mods = toWriter . Mod mods . Branch . runSymBuilder

-- | Monadic analogue of `Circle`
circle :: Float -> SymBuilder ()
circle = toWriter . Circle

-- | Monadic analogue of `Poly`
poly :: [Vec] -> SymBuilder ()
poly = toWriter . Poly
