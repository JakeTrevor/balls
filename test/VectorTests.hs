{-# LANGUAGE LambdaCase #-}

module VectorTests (vectorTests) where

import Test.QuickCheck.Assertions ((?==), (?~==))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Vectors (dist, dotProd, magnitude, perpendicular, pointAEq, project, scalarMul, unit, unitProject, vectorAdd, vectorSub)

almostTrue = 1 ?== 1

magTest :: TestTree
magTest = testProperty "|v * c| = |v| * c" $
  \v c -> (abs c * magnitude v) ?~== magnitude (scalarMul v c)

doubling :: TestTree
doubling = testProperty "|v + v| = |v| * 2" $
  \v -> magnitude (vectorAdd v v) == 2 * magnitude v

doubling2 :: TestTree
doubling2 = testProperty "v + v = 2 * v" $
  \v -> vectorAdd v v == scalarMul v 2

addition :: TestTree
addition = testProperty "v + w = (vx + wx, vy + wy)" $
  \v1@(x1, y1) v2@(x2, y2) -> vectorAdd v1 v2 == (x1 + x2, y1 + y2)

subtraction :: TestTree
subtraction = testProperty "v + (-1 * w) = v - w" $
  \v w -> vectorSub v w == vectorAdd v (scalarMul w (-1))

dist1 :: TestTree
dist1 = testProperty "dist(v, v) = 0" $
  \v -> dist v v == 0

dist2 :: TestTree
dist2 = testProperty "dist(v, v+(0, 5)) = 5" $
  \v@(x, y) -> dist v (x, y + 5) ?~== 5

dotProd1 :: TestTree
dotProd1 = testProperty "dot(v, perpendicular(v)) = 0" $
  \v -> dotProd v (perpendicular v) == 0

dotProd2 :: TestTree
dotProd2 = testProperty "dot(v, v) = |v|^2" $
  \v -> sqrt (dotProd v v) == magnitude v

project1 :: TestTree
project1 =
  testProperty "project v -> v == 1" $
    \case
      (0, 0) -> almostTrue
      v -> project v v ?~== 1

project2 :: TestTree
project2 =
  testProperty "project v -> perpendicular(v) == 0" $
    \case
      (0, 0) -> almostTrue
      v -> project (perpendicular v) v ?~== 0

projectXY :: TestTree
projectXY = testProperty "project v -> x y = v" $
  \v -> pointAEq (unitProject (1, 0) (0, 1) v) v

projectYX :: TestTree
projectYX = testProperty "project v -> y x = flipped v" $
  \v@(x, y) -> pointAEq (unitProject (0, 1) (1, 0) v) (y, x)

unitLength :: TestTree
unitLength = testProperty "|unit(v)| = 1" $
  \case
    (0, 0) -> almostTrue
    v -> magnitude (unit v) ?~== 1

vectorTests :: TestTree
vectorTests =
  testGroup
    "vector tests"
    [ magTest,
      doubling,
      doubling2,
      addition,
      subtraction,
      dist1,
      dist2,
      dotProd1,
      dotProd2,
      project1,
      project2,
      projectXY,
      projectYX,
      unitLength
    ]