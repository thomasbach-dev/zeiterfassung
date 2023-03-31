module Zeiterfassung.Representation
  ( LogLine(..)
  , loggedTime
  , roundLogLine
  , Task
  , defaultRoundingTOD
  , defaultRoundingUTCT
  , todRoundToNextNMinutes
  , todRoundToNextFiveMinutes
  , todRoundToNextQuarterHours
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
roundLogLine l = l { startTime = defaultRoundingUTCT (startTime l)
                   , endTime = defaultRoundingUTCT (endTime l)
                   }

defaultRoundingTOD :: TimeOfDay -> TimeOfDay
defaultRoundingTOD = todRoundToNextQuarterHours

defaultRoundingUTCT :: UTCTime -> UTCTime
defaultRoundingUTCT (UTCTime d t) = (UTCTime d t')
  where
    t' = timeOfDayToTime . defaultRoundingTOD . timeToTimeOfDay $ t

todRoundToNextQuarterHours :: TimeOfDay -> TimeOfDay
todRoundToNextQuarterHours = todRoundToNextNMinutes 15

todRoundToNextFiveMinutes :: TimeOfDay -> TimeOfDay
todRoundToNextFiveMinutes = todRoundToNextNMinutes 5

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
