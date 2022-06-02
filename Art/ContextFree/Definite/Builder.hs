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
  ( SymWriter
  , branch
  , modify
  , circle
  , poly
  , runSymWriter
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

newtype SymWriter a = SymWriter { unSymWriter :: Writer (DList Symbol) a }
  deriving Functor

instance Apply SymWriter where
  SymWriter w <.> SymWriter w' = SymWriter $ w <*> w'

instance Bind SymWriter where
  SymWriter w >>- f = SymWriter $ w >>= fmap unSymWriter f

instance Semigroup a => Semigroup (SymWriter a) where
  SymWriter w <> SymWriter w' = SymWriter $ do
    a <- w
    b <- w'
    pure $ a <> b

-- | Run a Monadic symbol writer
runSymWriter :: SymWriter a -> NonEmpty Symbol
runSymWriter = NE.fromList . DL.toList . execWriter . unSymWriter

toWriter :: Symbol -> SymWriter ()
toWriter = SymWriter . tell . pure

-- | Monadic analogue of `Branch`
branch :: SymWriter a -> SymWriter ()
branch = toWriter . Branch . runSymWriter

-- | Monadic analogue of `Mod`
modify :: [Modifier] -> SymWriter a -> SymWriter ()
modify mods = toWriter . Mod mods . Branch . runSymWriter

-- | Monadic analogue of `Circle`
circle :: Float -> SymWriter ()
circle = toWriter . Circle

-- | Monadic analogue of `Poly`
poly :: [Vec] -> SymWriter ()
poly = toWriter . Poly
