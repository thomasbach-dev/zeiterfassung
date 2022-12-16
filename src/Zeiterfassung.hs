module Zeiterfassung
  ( readAndTransform
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec  as P

import Data.Time   (timeToDaysAndTimeOfDay)
import System.Exit (die)

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
