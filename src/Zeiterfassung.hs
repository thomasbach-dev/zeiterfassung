module Zeiterfassung
  ( readAndTransform
  , getConfigurationFilePath
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec  as P

import Data.Time          (timeToDaysAndTimeOfDay)
import System.Environment (getArgs, lookupEnv)
import System.Exit        (die)

import Zeiterfassung.Config
import Zeiterfassung.Parser
import Zeiterfassung.Representation
import Zeiterfassung.ToSpreadsheet

readAndTransform :: FilePath -> IO ()
readAndTransform configFile = do
  cfg <- readConfig configFile
  input <- TIO.getContents
  case P.parse pAgendaLog "" input of
    Left err     -> die $ show err
    Right logEntries -> do
      -- print parsed
      TIO.putStrLn $ toSpreadsheet cfg logEntries
      TIO.putStrLn ""
      let roundedDiff =  timeToDaysAndTimeOfDay . sum . map (loggedTime . roundLogLine) $ logEntries
          originalDiff = timeToDaysAndTimeOfDay . sum . map loggedTime $ logEntries

      TIO.putStrLn $ "Time logged: " <> T.pack (show originalDiff)
      TIO.putStrLn $ "Time rounded: " <> T.pack (show roundedDiff)

getConfigurationFilePath :: IO FilePath
getConfigurationFilePath = do
  envVar <- lookupEnv "UNV_ZEITERFASSUNG_CFG"
  progArgs <- getArgs
  case (progArgs, envVar) of
    ([cfgFile], _)    -> pure cfgFile
    (_, Just cfgFile) -> pure cfgFile
    _                 -> die . unwords $ helpText
  where
    helpText = [ "Please provide either exactlay one argument with the path to the configuration file"
               , "or set the UNV_ZEITERFASSUNG_CFG variable."
               ]
