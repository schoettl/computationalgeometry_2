{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt
import Data.List.Split

patterns :: Docopt
patterns = [docopt|
area version 1.0

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
  putStr content

  when (isPresent args (longOption "points")) $ do
    pointFile <- getArgOrExit args (longOption "points")
    putStrLn $ "--points is set: " ++ pointFile




data Point = Point { xCoord::Double, yCoord::Double } deriving (Show)
data Area = Area { areaName::String, polygons::[[Point]] } deriving (Show)
type UnparsedCommands = [(Char, [String])]
type ParsedCommands = [(Char, Point)]

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

splitPolygons :: [(String, UnparsedCommands)] -> [(String, [UnparsedCommands])]
splitPolygons = map (\(n, cmds) -> (n, endBy [('z', [])] cmds))

d :: [(String, [[Point]])]
d = (convert . splitPolygons . groupData . parseData . readData) exampleData

areas :: [(String, [[Point]])] -> [Area]
areas = map (\(s, l) -> Area { areaName = s, polygons = l })

convert :: [(String, [UnparsedCommands])] -> [(String, [[Point]])]
convert = map (\(s, l) -> (s, map convertSublist l))

convertSublist :: UnparsedCommands -> [Point]
convertSublist = interpreteList . convertToPoints

convertToNumbers :: UnparsedCommands -> [(Char, [Double])]
convertToNumbers = map (\(c, s) -> (c, map (read :: String -> Double) s))

convertToPoints :: [(Char, [String])] -> ParsedCommands
convertToPoints = map (\(c, d) -> (c, Point { xCoord= read $ d!!0 :: Double, yCoord=(read $ d!!1 :: Double) }))

interpreteList :: ParsedCommands -> [Point]
interpreteList = foldl interpreteCommand []
  where interpreteCommand a (c, p) = a ++ [p']
        p' = if c == 'l' then addPoints (last a) p else p

addPoints :: Point -> Point -> Point
addPoints p q = Point (xCoord p + xCoord q) (yCoord p + yCoord q)
