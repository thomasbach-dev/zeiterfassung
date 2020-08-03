module Zeiterfassung.Parser
  ( pAgendaLog
  , pLogLine
  , pDate
  ) where

import Control.Applicative ((<$))
import Data.Time (Day, fromGregorian)
import Text.Parsec.Text (Parser)

import qualified Text.Parsec as P
import qualified Data.Text as T

import Zeiterfassung.Data

pAgendaLog :: Parser AgendaLog
pAgendaLog = do _ <- P.optional (P.string "Week-agend" 
                                 *> P.anyChar `P.manyTill` P.newline)
                P.many ((,) <$> pDate
                            <*> (pLogLine `P.manyTill` ((const () <$> P.lookAhead pDate) P.<|> P.eof)))

pLogLine :: Parser LogLine
pLogLine = do _ <- P.spaces *> P.anyChar `P.manyTill` P.space *> P.spaces
              start <- pTime
              _ <- P.char '-' *> P.optional P.space
              end <- pTime
              _ <- P.space *> P.string "Clocked:" *> P.spaces
                   *> P.char '(' *> pTime *> P.char ')' *> P.spaces
              _ <- P.optional (pTaskState *> P.space)
              subj <- T.pack <$> P.anyChar `P.manyTill` P.newline
              return (LogLine start end subj)

pTime :: Parser Time
pTime = do hour <- read <$> P.digit `P.manyTill` P.char ':'
           minute <- read <$> P.count 2 P.digit
           return (Time hour minute)

pTaskState :: Parser String
pTaskState = (P.try . P.choice . map P.string) ["TODO", "DONE"]

pDate :: Parser Day
pDate = do _ <- pWeekday
           _ <- P.skipMany1 P.space
           day <- pDay
           _ <- P.space
           month <- pMonth
           _ <- P.space
           year <- pYear
           _ <- P.anyChar `P.manyTill` P.newline
           return (fromGregorian year month day)

pDay :: Parser Int
pDay = read <$> (P.many1 P.digit)

pMonth :: Parser Int
pMonth = 
  P.choice [ 7 <$ P.string "July"
           , 8 <$ P.string "August"
           ]

pYear :: Parser Integer
pYear = read <$> P.count 4 P.digit

pWeekday :: Parser String
pWeekday = P.choice $
  map (P.try . P.string) [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"
                         , "Saturday", "Sunday"]
