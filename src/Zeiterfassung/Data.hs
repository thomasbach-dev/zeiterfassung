module Zeiterfassung.Data
   ( AgendaLog
   , LogLine(..)
   , Time(..)
   , roundToNextFiveMinutes
   ) where

import qualified Data.Text as T

import Data.Time (Day)

type AgendaLog = [(Day, [LogLine])]

data LogLine = LogLine { startTime :: Time
                       , endTime :: Time
                       , subject :: T.Text
                       }
             deriving (Eq, Show)

data Time = Time Int Int
          deriving (Eq, Show)

roundToNextFiveMinutes :: Time -> Time
roundToNextFiveMinutes (Time h m) = (Time h' m'')
  where 
    remainer = m `mod` 5
    m' = if remainer < 3
            then m - remainer
            else m + (5 - remainer)
    (h', m'') = case m' of
                  60 -> (h + 1, 0)
                  _  -> (h, m')
