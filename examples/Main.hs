module Main where

import Art.ContextFree.Definite
import Spiral
import Circles
import Sierpinski.Triangle
import Sierpinski.Carpet

import Control.Arrow (second)
import qualified Data.Text.Lazy.IO as T
import System.Directory
import Text.Blaze.Svg.Renderer.Text

baseDir :: String
baseDir = "res"

imagePath :: String -> String
imagePath name = baseDir <> "/" <> name <> ".svg"

renderTmp :: String -> Symbol -> IO ()
renderTmp name symbol = do
  let path = imagePath name
  putStrLn $ "Writing " <> path
  createDirectoryIfMissing True baseDir
  T.writeFile path $ renderSvg $ render symbol

images :: [(String, Symbol)]
images = second (Branch . runSymBuilder) <$>
  [ ("spiral", spiral)
  , ("sierpinski-triangle", sierpinskiTriangle)
  , ("sierpinski-carpet", sierpinskiCarpet)
  , ("circles-1", modify [Rotate 90] $ circles 2 5 [])
  , ("circles-2", circles 3 5 [])
  , ("circles-3", circles 3 5 [Rotate 180])
  , ("circles-4", modify [Rotate 90] $ circles 2 4 [Scale 0.8])
  , ("circles-5", modify [Rotate 45] $ circles 4 4 [])
  ]

main :: IO ()
main = mapM_ (uncurry renderTmp) images
