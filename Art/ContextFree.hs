{-|
Module      : Art.ContextFree
Description : Generate art from context-free grammars
Copyright   : (c) Owen Shepherd, 2019
License     : BSD-3-Clause
Maintainer  : 414owen@gmail.com
Stability   : experimental

Create art via context free grammar production rules.
-}

module Art.ContextFree
  ( Modifier(..)
  , Symbol(..)
  , Vec
  , Production
  , interpret
  ) where

import Art.Grammar
import Art.Interpreter
import Art.Geometry
