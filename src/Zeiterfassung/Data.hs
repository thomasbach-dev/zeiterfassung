{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Zeiterfassung.Data
   ( AgendaLog
   , ToSpreadsheet(..)
   , LogLine(..)
   , Task(..)
   , Time(..)
   , roundToNextFiveMinutes
   ) where

import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, formatTime)

type AgendaLog = [(Day, [LogLine])]

class ToSpreadsheet a where
  toSpreadsheet :: a -> T.Text

instance ToSpreadsheet Day where
  toSpreadsheet = T.pack . formatTime defaultTimeLocale "%m/%d/%Y"

data LogLine = LogLine { startTime :: Time
                       , endTime :: Time
                       , subject :: T.Text
                       , task :: Task
                       }
             deriving (Eq, Show)

instance ToSpreadsheet LogLine where
  toSpreadsheet (LogLine {..}) =
    T.intercalate "," [ toSpreadsheet task
                      , toSpreadsheet startTime
                      , toSpreadsheet endTime
                      , ""
                      , subject
                      ]  

data Task = CONSULTING_ORG 
          | CONSULTING_Q4 
          | UPG_TO_44
          deriving (Eq, Show)

instance ToSpreadsheet Task where
  toSpreadsheet CONSULTING_ORG = "Consulting: Organisation"
  toSpreadsheet CONSULTING_Q4  = "OX IN8 - 2020 Q4 - consulting and project management"
  toSpreadsheet UPG_TO_44      = "OX IN8 - R6.45 - CR107/8 - Upgrade to UCS 4.4"

data Time = Time Int Int
          deriving (Eq, Show)


instance ToSpreadsheet Time where
  toSpreadsheet = formatTime' . roundToNextFiveMinutes 

formatTime' :: Time -> T.Text
formatTime' (Time h m) = T.pack (padZero h <> ":" <> padZero m)

padZero :: Int -> String
padZero = reverse . take 2 . reverse . ('0':) . show

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
