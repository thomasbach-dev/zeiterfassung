module Zeiterfassung where

import Control.Applicative ((<$))
import Data.Time (Day, fromGregorian)
import Text.Parsec.Text (Parser)

import qualified Text.Parsec as P

data LogLine = LogLine { startTime :: Time
                       , endTime :: Time
                       , subject :: String
                       }
             deriving (Eq, Show)

data Time = Time Int Int
          deriving (Eq, Show)

pLogLine :: Parser LogLine
pLogLine = do _ <- P.spaces *> P.anyChar `P.manyTill` P.space *> P.spaces
              start <- pTime
              _ <- P.char '-'
              end <- pTime
              _ <- P.space *> P.string "Clocked:" *> P.spaces
                   *> P.char '(' *> pTime *> P.char ')' *> P.spaces
              _ <- P.optional (pTaskState *> P.space)
              subj <- P.anyChar `P.manyTill` P.newline
              return (LogLine start end subj)

pTime :: Parser Time
pTime = do hour <- read <$> P.digit `P.manyTill` P.char ':'
           minute <- read <$> P.count 2 P.digit
           return (Time hour minute)

pTaskState :: Parser String
pTaskState = P.choice $ map P.string ["TODO", "DONE"]

pDate :: Parser Day
pDate = do _ <- pWeekday
           _ <- P.skipMany1 P.space
           day <- pDay
           _ <- P.space
           month <- pMonth
           _ <- P.space
           year <- pYear
           _ <- P.anyChar `P.manyTill` P.newline
           _ <- P.many P.newline
           return (fromGregorian year month day)

pDay :: Parser Int
pDay = read <$> P.count 2 P.digit

pMonth :: Parser Int
pMonth = 
  P.choice [ 7 <$ P.string "July"
           , 8 <$ P.string "August"
           ]

pYear :: Parser Integer
pYear = read <$> P.count 4 P.digit

pWeekday :: Parser String
pWeekday = P.choice $
  map P.string [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"
               , "Saturday", "Sunday"]
