{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad (when)
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docopt|
docopt-sample version 0.1.0

Usage:
  docopt-sample cat <file>
  docopt-sample echo [--caps=<test>] <string>

Options:
  -c, --caps=<test>    Caps-lock the echoed argument
|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "cat")) $ do
    file <- args `getArgOrExit` (argument "file")
    putStr =<< readFile file

  when (args `isPresent` (command "echo")) $ do
    let charTransform = if args `isPresent` (longOption "caps")
                        then toUpper
                        else id
    string <- args `getArgOrExit` (argument "string")
    putStrLn $ map charTransform string
    when (args `isPresent` (longOption "caps")) $ do
      a <- args `getArgOrExit` (longOption "caps")
      putStrLn $ "arg for caps: " ++ a
