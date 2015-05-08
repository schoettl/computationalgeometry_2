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
  putStr =<< readFile areaFile -- = cat

  when (isPresent args (longOption "points")) $ do
    pointFile <- getArgOrExit args (longOption "points")
    putStrLn $ "--points is set: " ++ pointFile




exampleData = "n abc\nM 1 2\nl 2 3\nL 1 2\nz\nM 1 2\nl 0 5\nz\nn efg\nM 0 0\nl 2 3\nz"

readData :: String -> [[String]]
readData = (map words) . lines

parseData :: [[String]] -> [(Char, [String])]
parseData = map (\(x:xs) -> (head x, xs))

data Point = Point { xCoord::Double, yCoord::Double }
data Area = Area { areaName::String, polygons::[[Point]] }

groupData :: [(Char, [String])] -> [(String, [(Char, [String])])]
groupData list = let
  names = map (last . snd) $ filter isName list
  cmds  = tail $ splitWhen isName list
  in zip names cmds
  where isName (c, _) = c == 'n'

splitPolygons :: [(String, [(Char, [String])])] -> [(String, [[(Char, [String])]])]
splitPolygons = map (\(n, cmds) -> (n, endBy [('z', [])] cmds))

convertNumbers = map (\(n, ps) -> (n, map (map convert) ps))
  where convert (c, s) = (c, map (read :: String -> Double) s)

d = (splitPolygons . groupData . parseData . readData) exampleData
