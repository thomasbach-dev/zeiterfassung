module Zeiterfassung
  ( readAndTransform
  ) where

import qualified Data.Text.IO as TIO
import qualified Text.Parsec  as P

import System.Exit (die)

import Zeiterfassung.Config
import Zeiterfassung.Parser
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

