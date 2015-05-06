{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt

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
