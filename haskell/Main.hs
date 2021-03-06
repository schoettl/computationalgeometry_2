{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt
import CG.Basic
import CG.Intersect (intersect)
import CG.Polygon (calculatePolygonArea, numberOfIntersects, toLines)
import CG.CG2Data
import Data.List.Split (splitOn)

patterns :: Docopt
patterns = [docopt|area version 1.0

usage:
  area [ -p <pointfile> ] <areafile>
  area [ -q <point> ] <areafile>

options:
  -p, --points=<pointfile>  for each point in this file,
      test in which areas it is and output the result
  -q, --point=<point>  for this point, test in which areas
      it is and output the result (format: x,y e.g.  1.5,4.2)
|]

getArgOrExit = getArgOrExitWith patterns

main = do
  args <- parseArgsOrExit patterns =<< getArgs

  areaFile <- getArgOrExit args (argument "areafile")
  areas <- parseAreaDataFile areaFile
  printTuples $ zip areas (calculateAllAreas areas)

  when (isPresent args (longOption "points")) $ do
    pointFile <- getArgOrExit args (longOption "points")
    places <- parsePlaceDataFile pointFile
    putStrLn $ replicate 80 '='
    printTuples $ zip places $ map (areasContainingPlace areas) places

  when (isPresent args (longOption "point")) $ do
    pointStr <- getArgOrExit args (longOption "point")
    let splittedPointStr = splitOn "," pointStr
        point = Point { xCoord = read (splittedPointStr!!0) :: Double
                      , yCoord = read (splittedPointStr!!1) :: Double }
        place = Place { placeName = "Place", coordinates = point }
    putStrLn $ replicate 80 '='
    printTuples [(place, areasContainingPlace areas place)]


printTuples :: (Show a, Show b) => [(a, b)] -> IO ()
printTuples = mapM_ (\(a, b) -> putStrLn $ show a ++ " " ++ show b)


calculateAllAreas :: [Area] -> [Double]
calculateAllAreas = map calculateArea

calculateArea :: Area -> Double
calculateArea Area{polygons=ps} = abs $ sum $ map calculatePolygonArea ps

areasContainingPlace :: [Area] -> Place -> [Area]
areasContainingPlace as p = areasContainingPoint as (coordinates p)

areasContainingPoint :: [Area] -> Point -> [Area]
areasContainingPoint as p = filter (flip containsPoint p) as

containsPoint :: Area -> Point -> Bool
containsPoint a@(Area{polygons=polygons}) p = if not err 
  then odd $ sum ns 
  else error "containsPoint: cannot test. there is a horizontal line at y of point"
  where 
    ns = map (flip numberOfIntersects line) polygons
    line = (Point { xCoord = minXOfArea a - 1, yCoord = yCoord p }, p)
    err = any (isHorizontalLineAtY (yCoord p)) polygons

minXOfArea :: Area -> Double
minXOfArea Area{polygons=ps} = minimum $ map minX ps

minX :: Polygon -> Double
minX (p:ps) =  foldr (\p a -> min (xCoord p) a) (xCoord p) ps

isHorizontalLineAtY :: Double -> Polygon -> Bool
isHorizontalLineAtY y ps = foldr f False $ toLines ps
  where
    f :: Line -> Bool -> Bool
    f (Point{yCoord=y1}, Point{yCoord=y2}) a = a || y == y1 && y1 == y2
