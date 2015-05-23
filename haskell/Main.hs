{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt
import Data.List.Split
import CG.Basic
import CG.Intersect (intersect)

patterns :: Docopt
patterns = [docopt|
area version 1.0

usage:
  area [-v] [ -p <pointfile> ] <areafile>

options:
  -p, --points=<pointfile>  for each point in this file,
      test in which area it is and output the result
  -v, --verbose  print the parsed Area data structure
|]

getArgOrExit = getArgOrExitWith patterns

main = do
  args <- parseArgsOrExit patterns =<< getArgs

  areaFile <- getArgOrExit args (argument "areafile")
  content <- readFile areaFile
  let areas = readAreas content
  mapM_ print $ calculateAllAreas areas

  when (isPresent args (longOption "verbose")) $ do
    print areas

  when (isPresent args (longOption "points")) $ do
    pointFile <- getArgOrExit args (longOption "points")
    putStrLn $ "--points is set: " ++ pointFile



type UnparsedCommands = [(Char, [String])]
type ParsedCommands = [(Char, Point)]
type AreaName = String

data Area = Area { areaName::AreaName, polygons::[[Point]] } deriving (Show)

exampleData = "n abc\nM 1 2\nl 2 3\nL 1 2\nz\nM 1 2\nl 0 5\nz\nn efg\nM 0 0\nl 2 3\nz"

readData :: String -> [[String]]
readData = (map words) . lines

parseData :: [[String]] -> UnparsedCommands
parseData = map (\(x:xs) -> (head x, xs))

groupData :: [(Char, [String])] -> [(String, UnparsedCommands)]
groupData list = let
      names = map (last . snd) $ filter isName list
      cmds  = tail $ splitWhen isName list
  in zip names cmds
    where isName (c, _) = c == 'n'

splitPolygons :: [(AreaName, UnparsedCommands)] -> [(AreaName, [UnparsedCommands])]
splitPolygons = map (\(n, cmds) -> (n, endBy [('z', [])] cmds))

convert :: [(AreaName, [UnparsedCommands])] -> [(AreaName, [[Point]])]
convert = map (\(s, l) -> (s, map convertSublist l))

asAreas :: [(AreaName, [[Point]])] -> [Area]
asAreas = map (\(s, l) -> Area { areaName = s, polygons = l })

convertSublist :: UnparsedCommands -> [Point]
convertSublist = interpreteList . convertToPoints

convertToPoints :: [(Char, [String])] -> ParsedCommands
convertToPoints = map (\(c, d) ->
        (c, Point { xCoord = read $ d!!0 :: Double
                  , yCoord = read $ d!!1 :: Double
                  }))

interpreteList :: ParsedCommands -> [Point]
interpreteList = foldl interpreteCommand []
  where interpreteCommand a (c, p) = a ++ [p']
          where p'
                  | c == 'l'  = addPoints (last a) p
                  | c == 'H'  = Point { xCoord = xCoord p
                                      , yCoord = yCoord (last a) }
                  | otherwise = p

addPoints :: Point -> Point -> Point
addPoints p q = Point (xCoord p + xCoord q) (yCoord p + yCoord q)

d :: [Area]
d = (asAreas . convert . splitPolygons . groupData . parseData . readData) exampleData

readAreas :: String -> [Area]
readAreas = asAreas . convert . splitPolygons . groupData . parseData . readData

calculateAllAreas :: [Area] -> [Double]
calculateAllAreas = map calculateArea

calculateArea :: Area -> Double
calculateArea Area {polygons=ps} = abs $ sum $ map calculatePolygonArea ps

calculatePolygonArea :: [Point] -> Double
calculatePolygonArea ps = fst $
        foldl (\(s, p') p -> (s + calculateTriangleArea p' p, p))
            (0, head ps) (tail ps)

calculateTriangleArea :: Point -> Point -> Double
calculateTriangleArea p q = 0.5 * det p q

det :: Point -> Point -> Double
det p q = xCoord p * yCoord q - xCoord q * yCoord p

testArea1 = Area { areaName = "Otterfing"
                 , polygons = [
                 [ Point { xCoord = 1, yCoord = 3 } -- ccw
                 , Point { xCoord = 3, yCoord = 1 }
                 , Point { xCoord = 5, yCoord = 3 }
                 , Point { xCoord = 3, yCoord = 4 }
                 , Point { xCoord = 2.5, yCoord = 5 }
                 , Point { xCoord = 1, yCoord = 3 }
                 ],
                 [ Point { xCoord = 2, yCoord = 3 } -- cw
                 , Point { xCoord = 3, yCoord = 3 }
                 , Point { xCoord = 3, yCoord = 2 }
                 , Point { xCoord = 2, yCoord = 3 }
                 ]]}

testArea2 = Area { areaName = "Jakobs Haus"
                 , polygons = [
                 [ Point { xCoord = 2, yCoord = 3 } -- cw
                 , Point { xCoord = 3, yCoord = 3 }
                 , Point { xCoord = 3, yCoord = 2 }
                 , Point { xCoord = 2, yCoord = 3 }
                 ]]}


minX :: Polygon -> Double
minX ps@(p:_) = foldr (\(Point {xCoord=x}) a -> min x a) (xCoord p) ps

minXOfArea :: Area -> Double
minXOfArea Area {polygons=ps} = minimum $ map minX ps

containsPoint :: Area -> Point -> Bool
containsPoint a@(Area {polygons=polygons}) p = odd $ sum $ map (flip numberOfIntersects line) polygons
  where line = (Point { xCoord = minXOfArea a - 1, yCoord = yCoord p }, p)

numberOfIntersects :: Polygon -> Line -> Int
numberOfIntersects polygon line = count (==True) $
    map (intersect line) $ toLines polygon

toLines :: Polygon -> [Line]
toLines ps@(_:ps') = zip ps ps'

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

isHorizontalLineAtY :: Polygon -> Double -> Bool
isHorizontalLineAtY ps y = foldr f False $ toLines ps
  where
    f :: Line -> Bool -> Bool
    f (Point{yCoord=y1}, Point{yCoord=y2}) a = a || y == y1 && y1 == y2
