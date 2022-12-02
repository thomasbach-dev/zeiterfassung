module Zeiterfassung.ToSpreadsheet
  ( ToSpreadsheet(..)
  ) where

import qualified Data.Map  as M
import qualified Data.Text as T

import Data.Time (Day, TimeOfDay (..), defaultTimeLocale, formatTime, timeToTimeOfDay, utctDayTime)

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
  toSpreadsheet cfg (day, logs) = T.unlines $ map (prefixDay . toSpreadsheet cfg) logs
   where
     prefixDay l = toSpreadsheet cfg day <> "," <> l

instance ToSpreadsheet Day where
  toSpreadsheet _ = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

instance ToSpreadsheet LogLine where
  toSpreadsheet cfg LogLine {..} =
    T.intercalate "," [ toSpreadsheet cfg tasks
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

instance ToSpreadsheetFormat TimeOfDay where
  toSpreadsheetFormat = formatTime' . todRoundToNextFiveMinutes

formatTime' :: TimeOfDay -> T.Text
formatTime' (TimeOfDay h m _) = T.pack (padZero h <> ":" <> padZero m)

padZero :: Int -> String
padZero = reverse . take 2 . reverse . ('0':) . show
