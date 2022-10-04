module Main where

import Data.List          (intercalate)
import System.Environment (getArgs, lookupEnv)
import System.Exit        (die)

import Zeiterfassung

main :: IO ()
main = do
  envVar <- lookupEnv "UNV_ZEITERFASSUNG_CFG"
  progArgs <- getArgs
  cfgFile <- case (progArgs, envVar) of
               ([cfgFile], _)    -> pure cfgFile
               (_, Just cfgFile) -> pure cfgFile
               _                 -> die . intercalate " " $ helpText
  readAndTransform cfgFile
  where
    helpText = [ "Please provide either exactlay one argument with the path to the configuration file"
               , "or set the UNV_ZEITERFASSUNG_CFG variable."
               ]

