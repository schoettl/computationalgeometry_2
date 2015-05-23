{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt
import CG.Basic
import CG.Intersect (intersect)
import CG.Polygon (calculatePolygonArea, numberOfIntersects)
import CG.CG2

patterns :: Docopt
patterns = [docopt|area version 1.0

usage:
  area [ -p <pointfile> ] <areafile>

options:
  -p, --points=<pointfile>  for each point in this file,
      test in which area it is and output the result
|]

getArgOrExit = getArgOrExitWith patterns

main = do
  args <- parseArgsOrExit patterns =<< getArgs

  areaFile <- getArgOrExit args (argument "areafile")
  content <- readFile areaFile
  let areas = parseAreaData content
  printTuples $ zip areas (calculateAllAreas areas)

  when (isPresent args (longOption "points")) $ do
    pointFile <- getArgOrExit args (longOption "points")
    content <- readFile pointFile
    let places = parsePlaceData content
    putStrLn $ replicate 80 '='
    printTuples $ zip places $ map (areasContainingPlace areas) places



printTuples :: (Show a, Show b) => [(a, b)] -> IO ()
printTuples = mapM_ (\(a, b) -> putStrLn $ show a ++ " " ++ show b)


calculateAllAreas :: [Area] -> [Double]
calculateAllAreas = map calculateArea

calculateArea :: Area -> Double
calculateArea Area{polygons=ps} = abs $ sum $ map calculatePolygonArea ps


areasContainingPoint :: [Area] -> Point -> [Area]
areasContainingPoint as p = filter (flip containsPoint p) $ as

areasContainingPlace :: [Area] -> Place -> [Area]
areasContainingPlace as p = areasContainingPoint as (coordinates p)

containsPoint :: Area -> Point -> Bool
containsPoint a@(Area{polygons=polygons}) p = odd $ sum $ map (flip numberOfIntersects line) polygons
  where line = (Point { xCoord = minXOfArea a - 1, yCoord = yCoord p }, p)

minXOfArea :: Area -> Double
minXOfArea Area{polygons=ps} = minimum $ map minX ps

minX :: Polygon -> Double
minX ps@(p:_) = foldr (\Point{xCoord=x} a -> min x a) (xCoord p) ps
