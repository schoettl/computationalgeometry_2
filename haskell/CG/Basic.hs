module CG.Basic
  ( Point (..)
  , Polygon
  , Line
  , addPoints
  ) where

data Point = Point { xCoord::Double, yCoord::Double } deriving (Show)

type Polygon = [Point]
type Line = (Point, Point)

addPoints :: Point -> Point -> Point
addPoints p q = Point (xCoord p + xCoord q) (yCoord p + yCoord q)
