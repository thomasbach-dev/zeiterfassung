module Zeiterfassung.ToSpreadsheet
  ( ToSpreadsheet(..)
  ) where

import qualified Data.Map  as M
import qualified Data.Text as T

import Data.Time (Day, TimeOfDay (..), UTCTime (..), defaultTimeLocale, formatTime, timeToTimeOfDay)

import Data.Maybe                   (fromMaybe)
import Zeiterfassung.Config
import Zeiterfassung.Representation

class ToSpreadsheet a where
  toSpreadsheet :: Config -> a -> T.Text

class ToSpreadsheetFormat a where
  toSpreadsheetFormat :: a -> T.Text

instance ToSpreadsheet AgendaLog where
  toSpreadsheet cfg = T.concat . map (toSpreadsheet cfg)

instance ToSpreadsheet (Day, [LogLine]) where
  toSpreadsheet cfg (_, logs) = T.unlines $ map (toSpreadsheet cfg) logs

instance ToSpreadsheet LogLine where
  toSpreadsheet cfg LogLine {..} =
    T.intercalate "," [ toSpreadsheetFormat $ utctDay startTime
                      , toSpreadsheet cfg tasks
                      , formatOnlyTime startTime
                      , formatOnlyTime endTime
                      , ""
                      , subject
                      ]
    where
      formatOnlyTime = toSpreadsheetFormat . timeToTimeOfDay . utctDayTime

instance ToSpreadsheet [Task] where
  toSpreadsheet _ []       = ""
  toSpreadsheet cfg (t:ts) = fromMaybe (toSpreadsheet cfg ts) $ M.lookup t cfg

instance ToSpreadsheetFormat Day where
  toSpreadsheetFormat = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

instance ToSpreadsheetFormat TimeOfDay where
  toSpreadsheetFormat = formatTime' . todRoundToNextFiveMinutes

formatTime' :: TimeOfDay -> T.Text
formatTime' (TimeOfDay h m _) = T.pack (padZero h <> ":" <> padZero m)

padZero :: Int -> String
padZero = reverse . take 2 . reverse . ('0':) . show
