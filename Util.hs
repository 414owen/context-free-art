{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Tuple.Extra
import qualified Data.Text as T
import TextShow
import Geometry

tupLst :: (a, a) -> [a]
tupLst (a, b) = [a, b]

toPath :: [Vec] -> T.Text
toPath pts = T.intercalate " l" (T.unwords . tupLst . both showt <$> pts) <> "Z"
