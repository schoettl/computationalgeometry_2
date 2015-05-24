module CG.Polygon
  ( calculatePolygonArea
  , calculateTriangleArea
  , numberOfIntersects
  , toLines
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
