module Zeiterfassung.Data
   ( LogLine(..)
   , Time(..)
   ) where

data LogLine = LogLine { startTime :: Time
                       , endTime :: Time
                       , subject :: String
                       }
             deriving (Eq, Show)

data Time = Time Int Int
          deriving (Eq, Show)
