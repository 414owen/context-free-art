{-# LANGUAGE OverloadedStrings #-}

module Art.Util where

import qualified Data.Text as T
import TextShow
import Art.Geometry

tupLst :: (a, a) -> [a]
tupLst (a, b) = [a, b]

toPath :: [Vec] -> T.Text
toPath pts = T.intercalate " l" (T.unwords . tupLst . both showt <$> pts) <> "Z"
