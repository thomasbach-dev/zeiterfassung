{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zeiterfassung.Data
   ( AgendaLog
   , ToSpreadsheet(..)
   , LogLine(..)
   , Task(..)
   , Time(..)
   , roundToNextFiveMinutes
   ) where

import qualified Data.Text as T
import           Data.Time (Day, defaultTimeLocale, formatTime)

type AgendaLog = [(Day, [LogLine])]

class ToSpreadsheet a where
  toSpreadsheet :: a -> T.Text

instance ToSpreadsheet Day where
  toSpreadsheet = T.pack . formatTime defaultTimeLocale "%m/%d/%Y"

data LogLine = LogLine { startTime :: Time
                       , endTime   :: Time
                       , subject   :: T.Text
                       , task      :: Task
                       }
             deriving (Eq, Show)

instance ToSpreadsheet LogLine where
  toSpreadsheet LogLine {..} =
    T.intercalate "," [ toSpreadsheet task
                      , toSpreadsheet startTime
                      , toSpreadsheet endTime
                      , ""
                      , subject
                      ]

data Task = CONSULTING_ORG
          | CONSULTING_Q4
          | UPG_TO_44
          | ADMIN1
          | ADMIN2
          | ADMIN3
          | SOAP5
          | BPSOrder
          | MailDeletionFlow
          deriving (Eq, Show)

instance ToSpreadsheet Task where
  toSpreadsheet CONSULTING_ORG   = "Consulting: Organisation"
  toSpreadsheet CONSULTING_Q4    = "OX IN8 - 2020 Q4 - consulting and project management"
  toSpreadsheet UPG_TO_44        = "OX IN8 - R6.45 - CR107/8 - Upgrade to UCS 4.4"
  toSpreadsheet ADMIN1           = "OX IN8 - R6.11 - CR205/ADMIN-1: Admin Portal - Administration of storage on Site"
  toSpreadsheet ADMIN2           = "OX IN8 - R6.11 - CR205/ADMIN-2: Admin Portal - Administration of storage on Mailbox"
  toSpreadsheet ADMIN3           = "OX IN8 - R6.11 - CR205/ADMIN-3: Admin Portal - Massprov"
  toSpreadsheet SOAP5            = "OX IN8 - R6.11 - CR205/SOAP-5: Soap - mailBoxInformation"
  toSpreadsheet BPSOrder         = "OX IN8 - R6.11 - CR213/INU-2505: bps order through IOSW"
  toSpreadsheet MailDeletionFlow = "OX IN8 - R6.11 - CR225/INU-3043: Mailbox deletion flow"

data Time = Time Int Int
          deriving (Eq, Show)


instance ToSpreadsheet Time where
  toSpreadsheet = formatTime' . roundToNextFiveMinutes

formatTime' :: Time -> T.Text
formatTime' (Time h m) = T.pack (padZero h <> ":" <> padZero m)

padZero :: Int -> String
padZero = reverse . take 2 . reverse . ('0':) . show

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
