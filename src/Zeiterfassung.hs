module Zeiterfassung
  ( readAndTransform
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec  as P

import Data.Time   (timeToTimeOfDay)
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
    Right parsed -> do
      -- print parsed
      TIO.putStrLn $ toSpreadsheet cfg parsed
      TIO.putStrLn ""
      let roundedDiff =  timeToTimeOfDay . foldl1 (+) . map (loggedTime . roundLogLine) $ logEntries
          originalDiff = timeToTimeOfDay . foldl1 (+) . map loggedTime $ logEntries
          logEntries = concatMap snd parsed
      TIO.putStrLn $ "Time logged: " <> T.pack (show originalDiff)
      TIO.putStrLn $ "Time rounded: " <> T.pack (show roundedDiff)
