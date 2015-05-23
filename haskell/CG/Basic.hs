module CG.Basic
  ( Point (..)
  , Polygon
  , Line
  ) where

data Point = Point { xCoord::Double, yCoord::Double } deriving (Show)

type Polygon = [Point]
type Line = (Point, Point)
