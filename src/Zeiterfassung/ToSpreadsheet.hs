module Zeiterfassung.ToSpreadsheet
  ( ToSpreadsheet(..)
  ) where

import qualified Data.Text as T

import Data.Time (Day, defaultTimeLocale, formatTime)

import Zeiterfassung.Representation

class ToSpreadsheet a where
  toSpreadsheet :: a -> T.Text

instance ToSpreadsheet AgendaLog where
  toSpreadsheet = T.concat . map toSpreadsheet

instance ToSpreadsheet (Day, [LogLine]) where
  toSpreadsheet (day, logs) = T.unlines $ map (prefixDay . toSpreadsheet) logs
    where
      prefixDay l = toSpreadsheet day <> "," <> l

instance ToSpreadsheet Day where
  toSpreadsheet = T.pack . formatTime defaultTimeLocale "%d.%m.%Y"

instance ToSpreadsheet LogLine where
  toSpreadsheet LogLine {..} =
    T.intercalate "," [ toSpreadsheet task
                      , toSpreadsheet startTime
                      , toSpreadsheet endTime
                      , ""
                      , subject
                      ]

instance ToSpreadsheet Task where
  toSpreadsheet CONSULTING_Q1_2022 = "OX IN8 - 2022 Q1 - consulting and project management"
  toSpreadsheet WARRANTY =
    "OX NewMail (32129): R6.45.1 - Gewährleistung/Eskalation für R6.45.0 - abrechenbar (32129)"

instance ToSpreadsheet Time where
  toSpreadsheet = formatTime' . roundToNextFiveMinutes

formatTime' :: Time -> T.Text
formatTime' (Time h m) = T.pack (padZero h <> ":" <> padZero m)

padZero :: Int -> String
padZero = reverse . take 2 . reverse . ('0':) . show
