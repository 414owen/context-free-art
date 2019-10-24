{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty
import Test.HUnit hiding (path)
import Text.Blaze
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Art.Grammar
import Art.Geometry
import Art.Interpreter
import Art.Util

toSvg :: [Float] -> S.Svg -> S.Svg
toSvg bound = S.docTypeSvg
  ! A.version "1.1"
  ! A.viewbox (toValue $ unwords $ show <$> bound)

testRender :: String -> Symbol -> [Float] -> S.Svg -> Test
testRender desc start bound expected = TestCase $ do
  result <- interpret start
  assertEqual desc (renderSvg $ toSvg bound expected) (renderSvg result)

circle :: Float -> Vec -> S.Svg
circle r (x, y)
  = S.circle
  ! A.r  (toValue r)
  ! A.cx (toValue x)
  ! A.cy (toValue y)

path :: [Vec] -> S.Svg
path pts = S.path ! A.d (toValue $ toPath pts)

rendersCircle :: Test
rendersCircle
  = testRender "circle" circleSym [-1, -1, 2, 2]
      $ circle 1 (0, 0)
    where
      circleSym = Circle 1

rendersCircleWithRadius :: Test
rendersCircleWithRadius
  = testRender "circlewith radius" circleSym [-2, -2, 4, 4]
      $ circle 2 (0, 0)
    where
      circleSym = Circle 2

translatedCircle :: Test
translatedCircle
  = testRender "translated circle" a [5, 5, 2, 2]
      $ circle 1 (6, 6)
    where
      a = Mod [Move (6, 6)] b
      b = Circle 1

scaledCircle :: Test
scaledCircle
  = testRender "scaled circle" a [-0.5, -0.5, 1, 1]
      $ circle 0.5 (0, 0)
    where
      a = Mod [Scale 0.5] b
      b = Circle 1

translatedScaledCircle :: Test
translatedScaledCircle
  = testRender "translated scaled circle" a [10, 10, 4, 4]
      $ circle 2 (12, 12)
    where
      a = Mod [Scale 2, Move (6, 6)] b
      b = Circle 1

scaledTranslatedCircle :: Test
scaledTranslatedCircle
  = testRender "scaled translated circle" a [4, 4, 4, 4]
      $ circle 2 (6, 6)
    where
      a = Mod [Move (6, 6), Scale 2] b
      b = Circle 1

multipleScaledTranslatedCircles :: Test
multipleScaledTranslatedCircles
  = testRender "multiple symbols under one non-terminal" a [10, 10, 20, 20]
      $ circle 2 (12, 12) >> circle 2 (28, 28)
    where
      a = Mod [Scale 2, Move (6, 6)] e
      b = Circle 1
      c = Mod [Scale 2, Move (4, 4)] d
      d = Circle 0.5
      e = NonTerminal $ (100, b) :| [(100, c)]

rendersPoly :: Test
rendersPoly
  = testRender "poly" a [0, 0, 2, 1]
    $ path [(0, 0), (1, 1), (1, -1)]
    where
      a = Poly [(1, 1), (1, -1)]

rendersPolyTranslated :: Test
rendersPolyTranslated
  = testRender "translated poly" a [1, 1, 3, 2]
    $ path [(1, 1), (1, 1), (1, 0)]
    where
      a = Mod [Move (1, 1)] b
      b = Poly [(1, 1), (1, 0)]

rendersPolyScaled :: Test
rendersPolyScaled
  = testRender "scaled poly" a [0, -3, 6, 6]
    $ path [(0, 0), (3, 3), (3, -6)]
    where
      a = Mod [Scale 3] b
      b = Poly [(1, 1), (1, -2)]

rendersPolyScaled2 :: Test
rendersPolyScaled2
  = testRender "another scaled poly" a [-4, 0, 4, 4]
    $ path [(0, 0), (-4, 4), (2, -4)]
    where
      a = Mod [Scale 2] b
      b = Poly [(-2, 2), (1, -2)]

fill :: Test
fill
  = testRender "fill" a [-1, -1, 2, 2]
    $ S.g ! A.fill "green" $ circle 1 (0, 0)
    where
      a = Mod [Color "green"] b
      b = Circle 1

rotate :: Test
rotate
  = testRender "rotation" a [-1, -1, 2, 2]
    $ circle 1 (0, 0)
    where
      a = Mod [Rotate 10] b
      b = Circle 1

rotateAndMove :: Test
rotateAndMove
  = testRender "rotate and move" a [0, -1, 2, 2]
    $ circle 1 (1, 0)
    where
      a = Mod [Rotate 90, Move (0, -1)] b
      b = Circle 1

rotateAndMove2 :: Test
rotateAndMove2
  = testRender "rotate and move" a [0, -1, 2, 3]
    $ circle 1 (1, 0) >> circle 1 (1, 1)
    where
      a = modif $ NonTerminal $ (100, Circle 1) :| [(100, modif $ Circle 1)]
      modif = Mod [Rotate 90, Move (0, -1)]

assertClose :: String -> Float -> Float -> Assertion
assertClose s a b = assertEqual s True $ abs (a - b) < 0.000001

testRotateVec :: Test
testRotateVec
  = let (x, y) = rotateZero 90 (0, -1)
    in TestCase $ do
      assertClose "rotate vec x" 1 x
      assertClose "rotate vec x" 0 y

testReflectVec :: Test
testReflectVec
  = TestCase $ assertEqual "reflect vec" (1, 1) $ reflectVec (-1, -1)

testSubVecs :: Test
testSubVecs
  = TestCase $ assertEqual "sub vec" (1, 2) $ subVecs (2, 4) (1, 2)

svgToText :: Test
svgToText = TestCase $ do
  res <- renderSvg <$> interpret (Circle 1)
  assertEqual "svg text generation" res $
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    <> "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n"
    <> "    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"
    <> "<svg xmlns=\"http://www.w3.org/2000/svg\" "
    <> "xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" "
    <> "viewBox=\"-1.0 -1.0 2.0 2.0\"><circle r=\"1.0\""
    <> " cx=\"0.0\" cy=\"0.0\" /></svg>"

tests :: Test
tests = TestList
  [ svgToText
  , rendersCircle
  , rendersCircleWithRadius
  , scaledCircle
  , translatedCircle
  , translatedScaledCircle
  , scaledTranslatedCircle
  , multipleScaledTranslatedCircles
  , rendersPoly
  , rendersPolyTranslated   -- 10
  , rendersPolyScaled
  , rendersPolyScaled2
  , fill
  , rotate
  , testRotateVec
  , testReflectVec
  , rotateAndMove
  , rotateAndMove2
  , testSubVecs
  ]

main :: IO Counts
main = runTestTT tests
