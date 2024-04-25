module Vectors
  ( Point,
    vectorAdd,
    vectorSub,
    scalarMul,
    magnitude,
    dist,
    unit,
    perpendicular,
    dotProd,
    project,
  )
where

import Graphics.Gloss (Point)

vectorAdd :: Point -> Point -> Point
vectorAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vectorSub :: Point -> Point -> Point
vectorSub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

scalarMul :: Point -> Float -> Point
scalarMul (x, y) s = (x * s, y * s)

magnitude :: Point -> Float
magnitude (x, y) = sqrt ((x ** 2) + (y ** 2))

dist :: Point -> Point -> Float
dist p1 p2 = magnitude $ vectorSub p1 p2

unit :: Point -> Point
unit p@(x, y) = (x', y')
  where
    mag = magnitude p
    x' = x / mag
    y' = y / mag

perpendicular :: Point -> Point
perpendicular (x, y) = (-y, x)

dotProd :: Point -> Point -> Float
dotProd (ax, ay) (bx, by) = (ax * bx) + (ay * by)

project :: Point -> Point -> Point -> Point
project xBasis yBasis p = (x', y')
  where
    x' = dotProd xBasis p
    y' = dotProd yBasis p