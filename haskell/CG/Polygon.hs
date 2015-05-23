module CG.Polygon
  ( calculatePolygonArea
  , calculateTriangleArea
  , numberOfIntersects
  ) where

import CG.Intersect
import CG.Basic

calculatePolygonArea :: [Point] -> Double
calculatePolygonArea ps = fst $
        foldl (\(s, p') p -> (s + calculateTriangleArea p' p, p))
            (0, head ps) (tail ps)

calculateTriangleArea :: Point -> Point -> Double
calculateTriangleArea p q = 0.5 * det p q

det :: Point -> Point -> Double
det p q = xCoord p * yCoord q - xCoord q * yCoord p


numberOfIntersects :: Polygon -> Line -> Int
numberOfIntersects polygon line = countTrueValues $
    map (intersect line) $ toLines polygon

toLines :: Polygon -> [Line]
toLines ps@(_:ps') = zip ps ps'

countTrueValues :: [Bool] -> Int
countTrueValues = length . filter (==True)

isHorizontalLineAtY :: Polygon -> Double -> Bool
isHorizontalLineAtY ps y = foldr f False $ toLines ps
  where
    f :: Line -> Bool -> Bool
    f (Point{yCoord=y1}, Point{yCoord=y2}) a = a || y == y1 && y1 == y2
