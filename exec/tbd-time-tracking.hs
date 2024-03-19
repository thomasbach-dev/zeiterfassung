module Main (main) where

import Options.Applicative (execParser)
import Zeiterfassung.CLI

main :: IO ()
main = do
  res <- execParser mainParser
  print res
