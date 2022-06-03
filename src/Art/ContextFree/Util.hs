{-# LANGUAGE OverloadedStrings #-}

module Art.ContextFree.Util where

import qualified Data.Text as T
import TextShow
import Art.ContextFree.Geometry

tupLst :: (a, a) -> [a]
tupLst (a, b) = [a, b]

toPath :: [Vec] -> T.Text
toPath pts = "M" <> T.intercalate "l" (T.unwords . tupLst . both showt <$> pts) <> "Z"
