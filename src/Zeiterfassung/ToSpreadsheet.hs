module Zeiterfassung.ToSpreadsheet
  ( ToSpreadsheet(..)
  ) where

import qualified Data.Text as T

import Data.Time (Day, defaultTimeLocale, formatTime)

import qualified Data.Map                     as M
import           Data.Maybe                   (fromMaybe)
import           Zeiterfassung.Config
import           Zeiterfassung.Representation

class ToSpreadsheet a where
  toSpreadsheet :: Config -> a -> T.Text

instance ToSpreadsheet AgendaLog where
  toSpreadsheet cfg = T.concat . map (toSpreadsheet cfg)

instance ToSpreadsheet (Day, [LogLine]) where
  toSpreadsheet cfg (day, logs) = T.unlines $ map (prefixDay . toSpreadsheet cfg) logs
   where
     prefixDay l = toSpreadsheet cfg day <> "," <> l

instance ToSpreadsheet Day where
  toSpreadsheet _ = T.pack . formatTime defaultTimeLocale "%d.%m.%Y"

instance ToSpreadsheet LogLine where
  toSpreadsheet cfg LogLine {..} =
    T.intercalate "," [ toSpreadsheet cfg task
                      , toSpreadsheet cfg startTime
                      , toSpreadsheet cfg endTime
                      , ""
                      , subject
                      ]

instance ToSpreadsheet Task where
  toSpreadsheet cfg task = fromMaybe "" $ M.lookup task cfg

instance ToSpreadsheet Time where
  toSpreadsheet _ = formatTime' . roundToNextFiveMinutes

formatTime' :: Time -> T.Text
formatTime' (Time h m) = T.pack (padZero h <> ":" <> padZero m)

padZero :: Int -> String
padZero = reverse . take 2 . reverse . ('0':) . show
