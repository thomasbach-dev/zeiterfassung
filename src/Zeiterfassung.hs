{-# LANGUAGE OverloadedStrings #-}
module Zeiterfassung
  ( readAndTransform
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec  as P

import Data.Time (defaultTimeLocale, formatTime)

import Zeiterfassung.Data
import Zeiterfassung.Parser

readAndTransform :: IO ()
readAndTransform = TIO.interact $ either (error . show) toSpreadsheetFormat . P.parse pAgendaLog ""

toSpreadsheetFormat :: AgendaLog -> T.Text
toSpreadsheetFormat [] = ""
toSpreadsheetFormat ((day,logs):rst) = formatted <> toSpreadsheetFormat rst
  where
    formatted = (T.unlines . map (((toSpreadsheet day <> ",") <>) . toSpreadsheet)) logs
