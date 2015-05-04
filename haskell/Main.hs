{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main = do
  args <- parseArgsOrExit patterns =<< getArgs

  areaFile <- getArgOrExit args (argument "areafile")
  putStr =<< readFile areaFile

  when (isPresent args (longOption "points")) $ do
    pointFile <- getArgOrExit args (argument "pointfile")
    putStrLn "--points is set"
