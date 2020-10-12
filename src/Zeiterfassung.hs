{-# LANGUAGE OverloadedStrings #-}
module Zeiterfassung
  ( readAndTransform
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as P

import Zeiterfassung.Data
import Zeiterfassung.Parser

readAndTransform :: IO ()
readAndTransform = 
  either (error . show) TIO.putStrLn =<< fmap toSpreadsheetFormat . P.parse pAgendaLog "" <$> TIO.getContents

toSpreadsheetFormat :: AgendaLog -> T.Text
toSpreadsheetFormat [] = ""
toSpreadsheetFormat ((day,logs):rst) = formatted <> toSpreadsheetFormat rst
  where 
    formatted = (T.unlines . map (((toSpreadsheet day <> ",") <>) . toSpreadsheet)) logs
