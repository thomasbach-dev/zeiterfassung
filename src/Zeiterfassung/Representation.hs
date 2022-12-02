module Zeiterfassung.Representation
  (  LogLine(..)
  , loggedTime
  , roundLogLine
  , Task
  , roundToNextFiveMinutes
  , todRoundToNextFiveMinutes
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
roundLogLine l = l { startTime = roundToNextFiveMinutes (startTime l)
                   , endTime = roundToNextFiveMinutes (endTime l)
                   }

roundToNextFiveMinutes :: UTCTime -> UTCTime
roundToNextFiveMinutes (UTCTime d t) = (UTCTime d t')
  where
    t' = timeOfDayToTime . todRoundToNextFiveMinutes . timeToTimeOfDay $ t


todRoundToNextFiveMinutes :: TimeOfDay -> TimeOfDay
todRoundToNextFiveMinutes (TimeOfDay h m _) = TimeOfDay h' m'' 0
  where
    remainer = m `mod` 5
    m' = if remainer < 3
            then m - remainer
            else m + (5 - remainer)
    (h', m'') = case m' of
                  60 -> (h + 1, 0)
                  _  -> (h, m')
