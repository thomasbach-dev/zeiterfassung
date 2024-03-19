module Zeiterfassung.ToSpreadsheet
  ( ToSpreadsheet (..),
  )
where

import qualified Data.Map                     as M
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import           Data.Time
    (Day, TimeOfDay (..), UTCTime (..), defaultTimeLocale, formatTime, timeToTimeOfDay)
import           Zeiterfassung.Config
import           Zeiterfassung.Representation

class ToSpreadsheet a where
  toSpreadsheet :: Config -> a -> T.Text

class ToSpreadsheetFormat a where
  toSpreadsheetFormat :: a -> T.Text

instance ToSpreadsheet [LogLine] where
  toSpreadsheet cfg = T.unlines . map (toSpreadsheet cfg)

instance ToSpreadsheet LogLine where
  toSpreadsheet cfg LogLine {..} =
    T.intercalate
      ","
      [ toSpreadsheetFormat $ utctDay startTime,
        "\"" <> toSpreadsheet cfg tasks <> "\"",
        formatOnlyTime startTime,
        formatOnlyTime endTime,
        "",
        subject
      ]
    where
      formatOnlyTime = toSpreadsheetFormat . timeToTimeOfDay . utctDayTime

instance ToSpreadsheet [Task] where
  toSpreadsheet cfg tasks = go tasks ""
    where
      go [] prev       = prev
      go (t : ts) prev = go ts $ fromMaybe prev (M.lookup t cfg)

instance ToSpreadsheetFormat Day where
  toSpreadsheetFormat = T.pack . formatTime defaultTimeLocale "%d.%m.%y"

instance ToSpreadsheetFormat TimeOfDay where
  toSpreadsheetFormat = formatTime'

formatTime' :: TimeOfDay -> T.Text
formatTime' (TimeOfDay h m _) = T.pack (padZero h <> ":" <> padZero m)

padZero :: Int -> String
padZero = reverse . take 2 . reverse . ('0' :) . show
