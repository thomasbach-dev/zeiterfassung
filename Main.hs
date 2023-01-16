module Main where

import Zeiterfassung

main :: IO ()
main = do
  cfgFile <- getConfigurationFilePath
  readAndTransform cfgFile

