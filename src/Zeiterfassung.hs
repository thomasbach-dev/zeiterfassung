module Zeiterfassung
  ( readAndTransform
  ) where

import qualified Data.Text.IO as TIO
import qualified Text.Parsec  as P

import System.Exit (die)

import Zeiterfassung.Data
import Zeiterfassung.Parser

readAndTransform :: IO ()
readAndTransform = do
  input <- TIO.getContents
  case P.parse pAgendaLog "" input of
    Left err     -> die $ show err
    Right parsed -> TIO.putStrLn $ toSpreadsheet parsed

