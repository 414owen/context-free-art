module Main where

import Art.ContextFree.Definite
import qualified Data.Text.Lazy.IO as T
import System.Directory
import Text.Blaze.Svg.Renderer.Text
import Spiral
import Sierpinski.Triangle
import Sierpinski.Carpet

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
images =
  [ ("spiral", spiral)
  , ("sierpinski-triangle", sierpinskiTriangle)
  , ("sierpinski-carpet", sierpinskiCarpet)
  ]

main :: IO ()
main = mapM_ (uncurry renderTmp) images
