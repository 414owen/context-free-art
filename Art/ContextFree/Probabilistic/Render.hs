{-# LANGUAGE LambdaCase #-} 

module Art.ContextFree.Probabilistic.Render ( render ) where

import Data.List.NonEmpty
import Data.Maybe
import System.Random
import qualified Art.ContextFree.Definite.Grammar as DG
import qualified Art.ContextFree.Definite.Render as DR
import qualified Art.ContextFree.Probabilistic.Grammar as PG
import qualified Text.Blaze.Svg11 as S

in100 :: Int -> Int
in100 = (`mod` 100) . abs

applyProb :: (Float, PG.Symbol) -> IO (Maybe DG.Symbol)
applyProb (n, g) = randomIO >>= toSym
  where
    toSym num = if in100 num < round n then convert g else pure Nothing

pureJ :: a -> IO (Maybe a)
pureJ = pure . Just

convert :: PG.Symbol -> IO (Maybe DG.Symbol)
convert (PG.NonTerminal syms)
  = let convertsM = (sequence $ applyProb <$> toList syms) :: IO [Maybe DG.Symbol]
        converts = (catMaybes <$> convertsM) :: IO [DG.Symbol]
    in  converts >>= \case
      [] -> pure Nothing
      (s : ss) -> pureJ $ DG.Branch $ s :| ss
convert (PG.Mod mods sym) = fmap (DG.Mod mods) <$> convert sym
convert (PG.Circle r) = pureJ $ DG.Circle r
convert (PG.Poly vecs) = pureJ $ DG.Poly vecs

render :: PG.Symbol -> IO (Maybe S.Svg)
render sym = fmap DR.render <$> convert sym
