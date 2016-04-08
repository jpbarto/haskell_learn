module Geometry (
    Point(..),
    Radius(..),
    Shape(..),
    sphereVolume,
    sphereArea,
    cuboidArea
) where

data Point = Point Float Float
data Radius = Radius Float
data Shape = Circle Point Radius | Rectangle Point Point

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

area :: Shape -> Float
area (Circle _ (Radius r)) = (4.0/3.0) * pi * (r ^ 3)
area (Rectangle (Point x1 y1) (Point x2 y2)) = (x1 - x2) * (y1 - y2)
