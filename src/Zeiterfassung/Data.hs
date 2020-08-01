module Zeiterfassung.Data
   ( AgendaLog
   , LogLine(..)
   , Time(..)
   , roundToNextFiveMinutes
   ) where

import Data.Time (Day)

type AgendaLog = [(Day, [LogLine])]

data LogLine = LogLine { startTime :: Time
                       , endTime :: Time
                       , subject :: String
                       }
             deriving (Eq, Show)

data Time = Time Int Int
          deriving (Eq, Show)
