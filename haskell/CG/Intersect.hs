module CG.Intersect where

import CG.Basic

intersect :: Line -> Line -> Bool
intersect this other
  | ccwFst * ccwSnd > 0 = False
  | ccwFst * ccwSnd < 0 = ccw other (fst this) * ccw other (snd this) <= 0
  | ccwFst == 0 && inRect this (fst other) = True
  | ccwSnd == 0 && inRect this (snd other) = True
  | otherwise = False
  where
    ccwFst = ccw this (fst other)
    ccwSnd = ccw this (snd other)

ccw :: Line -> Point -> Double
ccw
  ( Point {xCoord=px, yCoord=py}
  , Point {xCoord=qx, yCoord=qy} )
    Point {xCoord= x, yCoord= y}
  = (px*qy - py*qx) + (qx*y - qy*x) + (py*x - px*y)

inRect :: (Point, Point) -> Point -> Bool
inRect
  ( Point {xCoord=ax, yCoord=ay}
  , Point {xCoord=bx, yCoord=by} )
    Point {xCoord= x, yCoord= y}
  = inRange x (ax, bx) && inRange y (ay, by)

inRange :: (Num a, Ord a) => a -> (a, a) -> Bool
inRange x (a, b) = x >= min a b && x <= max a b
