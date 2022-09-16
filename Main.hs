module Main where

import System.Environment (getArgs)
import System.Exit        (die)

import Zeiterfassung

main :: IO ()
main = do
  progArgs <- getArgs
  case progArgs of
    [cfgFile] -> readAndTransform cfgFile
    _         -> die "Please provide exactlay one argument with the path to the configuration file"

