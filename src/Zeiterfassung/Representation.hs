module Zeiterfassung.Representation
  (  LogLine(..)
  , loggedTime
  , roundLogLine
  , Task
  , roundToNextFiveMinutes
  , todRoundToNextFiveMinutes
  , roundToNextQuarterHours
  , todRoundToNextQuarterHours
  , roundToNextNMinutes
  ) where

import qualified Data.Text as T

import Data.Time
    (NominalDiffTime, TimeOfDay (..), UTCTime (..), diffUTCTime, timeOfDayToTime, timeToTimeOfDay)

data LogLine = LogLine
  { startTime :: UTCTime
  , endTime   :: UTCTime
  , subject   :: T.Text
  , tasks     :: [Task]
  } deriving (Eq, Show)

type Task = T.Text

loggedTime :: LogLine -> NominalDiffTime
loggedTime LogLine{..} = diffUTCTime endTime startTime

roundLogLine :: LogLine -> LogLine
roundLogLine l = l { startTime = rounding (startTime l)
                   , endTime = rounding (endTime l)
                   }
  where
    rounding = roundToNextFiveMinutes

roundToNextQuarterHours :: UTCTime -> UTCTime
roundToNextQuarterHours = roundToNextNMinutes 15

todRoundToNextQuarterHours :: TimeOfDay -> TimeOfDay
todRoundToNextQuarterHours = todRoundToNextNMinutes 15

roundToNextFiveMinutes :: UTCTime -> UTCTime
roundToNextFiveMinutes (UTCTime d t) = (UTCTime d t')
  where
    t' = timeOfDayToTime . todRoundToNextFiveMinutes . timeToTimeOfDay $ t

todRoundToNextFiveMinutes :: TimeOfDay -> TimeOfDay
todRoundToNextFiveMinutes = todRoundToNextNMinutes 5

roundToNextNMinutes :: Int -> UTCTime -> UTCTime
roundToNextNMinutes n (UTCTime d t) = (UTCTime d t')
  where
    t' = timeOfDayToTime . todRoundToNextNMinutes n . timeToTimeOfDay $ t

todRoundToNextNMinutes :: Int -> TimeOfDay -> TimeOfDay
todRoundToNextNMinutes n (TimeOfDay h m _) = TimeOfDay h' m'' 0
  where
    (quotient, remainder) = m `divMod` n
    fact = if remainder <= (n `div` 2)
             then quotient
             else quotient + 1
    m' = fact * n
    (h', m'') = case m' of
                  60 -> (h + 1, 0)
                  _  -> (h, m')
