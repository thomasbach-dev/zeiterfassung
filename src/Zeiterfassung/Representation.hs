module Zeiterfassung.Representation
  ( AgendaLog
   , LogLine(..)
   , loggedTime
   , roundLogLine
   , Task
   , Time(..)
   , roundToNextFiveMinutes
   , timeDiff
   ) where

import qualified Data.Text as T

import Data.Time (Day, DiffTime, secondsToDiffTime)

type AgendaLog = [(Day, [LogLine])]

data LogLine = LogLine
  { startTime :: Time
  , endTime   :: Time
  , subject   :: T.Text
  , tasks     :: [Task]
  } deriving (Eq, Show)

type Task = T.Text

loggedTime :: LogLine -> DiffTime
loggedTime LogLine{..} = timeDiff startTime endTime

roundLogLine :: LogLine -> LogLine
roundLogLine l = l { startTime = roundToNextFiveMinutes (startTime l)
                   , endTime = roundToNextFiveMinutes (endTime l)
                   }

data Time = Time Word Word
          deriving (Eq, Show)

instance Semigroup Time where
  (Time h1 m1) <> (Time h2 m2) = Time h m
    where
      h = h1 + h2 + hOverflow
      (hOverflow, m) = (m1 + m2) `divMod` 60

instance Monoid Time where
  mempty = Time 0 0

timeDiff :: Time -> Time -> DiffTime
timeDiff (Time h1 m1) (Time h2 m2) = secondsToDiffTime $ totalDiff * 60
  where
    minutesDiff = conv m2 - conv m1
    hoursDiff = conv h2 - conv h1
    totalDiff = hoursDiff * 60 + minutesDiff
    conv = toInteger

roundToNextFiveMinutes :: Time -> Time
roundToNextFiveMinutes (Time h m) = Time h' m''
  where
    remainer = m `mod` 5
    m' = if remainer < 3
            then m - remainer
            else m + (5 - remainer)
    (h', m'') = case m' of
                  60 -> (h + 1, 0)
                  _  -> (h, m')
