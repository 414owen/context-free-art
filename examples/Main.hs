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

maskTest :: SymBuilder
maskTest = circle 1 !> (circle 0.5 !> circle 0.3)

images :: [(String, Symbol)]
images = second (Branch . runSymBuilder) <$>
  [ ("spiral", spiral)
  , ("sierpinski-triangle", sierpinskiTriangle)
  , ("sierpinski-carpet", sierpinskiCarpet)
  , ("circles-1", circles 2 5 [] ! Rotate 90)
  , ("circles-2", circles 3 5 [])
  , ("circles-3", circles 3 5 [Rotate 180])
  , ("circles-4", circles 2 4 [Scale 0.8] ! Rotate 90)
  , ("circles-5", circles 4 4 [] ! Rotate 45)
  , ("mask-test", maskTest)
  ]

main :: IO ()
main = mapM_ (uncurry renderTmp) images
