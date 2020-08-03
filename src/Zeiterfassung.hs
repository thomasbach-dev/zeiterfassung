{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Zeiterfassung
  ( readAndTransform
  ) where

import Data.Time (defaultTimeLocale, formatTime)

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
    formatted = (T.unlines . map formatLine) logs
    fDate = (T.pack . formatTime defaultTimeLocale "%m/%d/%Y") day
    formatLine (LogLine {..}) =
      T.intercalate "," [ fDate
                        , ""
                        , roundAndFormatTime startTime
                        , roundAndFormatTime endTime
                        , ""
                        , subject
                        ]

roundAndFormatTime :: Time -> T.Text
roundAndFormatTime = formatTime' . roundToNextFiveMinutes

formatTime' :: Time -> T.Text
formatTime' (Time h m) = T.pack (padZero h <> ":" <> padZero m)

padZero :: Int -> String
padZero = reverse . take 2 . reverse . ('0':) . show
