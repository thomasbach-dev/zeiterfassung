{-# LANGUAGE OverloadedStrings #-}
module Zeiterfassung
  ( readAndTransform
  ) where

import qualified Data.Text    as T
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
    Right parsed -> TIO.putStrLn $ toSpreadsheetFormat parsed

toSpreadsheetFormat :: AgendaLog -> T.Text
toSpreadsheetFormat [] = ""
toSpreadsheetFormat ((day,logs): rst) = formatted <> toSpreadsheetFormat rst
  where
    formatted = T.unlines $ map (prefixDay . toSpreadsheet) logs
    prefixDay l = toSpreadsheet day <> "," <> l
