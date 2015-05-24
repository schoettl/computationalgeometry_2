module CG.CG2Data
  ( parseAreaDataFile
  , parsePlaceDataFile
  , Area (..)
  , Place (..)
  ) where

import Data.List.Split
import CG.Basic

type UnparsedCommands = [(Char, [String])]
type ParsedCommands = [(Char, Point)]
type AreaName = String
type PlaceName = String

data Area = Area { areaName::AreaName, polygons::[Polygon] }
data Place = Place { placeName::PlaceName, coordinates::Point }

instance Show Area where show = areaName
instance Show Place where show = placeName


parseAreaDataFile :: FilePath -> IO [Area]
parseAreaDataFile f = fmap parseAreaData (readFile f)
                       
parseAreaData :: String -> [Area]
parseAreaData = asAreas . convert . splitPolygons . groupData . parseData . readData


parsePlaceDataFile :: FilePath -> IO [Place]
parsePlaceDataFile f = fmap parsePlaceData (readFile f)

parsePlaceData :: String -> [Place]
parsePlaceData rawdata =
  let
    tokens = map words $ lines rawdata
  in map (\(n:x:y:[]) ->
              Place { placeName = n
                    , coordinates = Point { xCoord = read x :: Double
                                          , yCoord = read y :: Double }}
         ) tokens




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


exampleData = "n abc\nM 1 2\nl 2 3\nL 1 2\nz\nM 1 2\nl 0 5\nz\nn efg\nM 0 0\nl 2 3\nz"


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
