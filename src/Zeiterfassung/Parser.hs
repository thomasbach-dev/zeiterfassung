module Zeiterfassung.Parser
  ( pAgendaLog
  , pLogLine
  , pClockedTask
  , pDate
  , pTasksFromTags
  ) where

import qualified Data.Text   as T
import qualified Text.Parsec as P

import Control.Monad    (void)
import Data.Maybe       (catMaybes)
import Data.Time        (Day, fromGregorian)
import Text.Parsec.Text (Parser)

import Zeiterfassung.Representation

pAgendaLog :: Parser AgendaLog
pAgendaLog = do _ <- P.optional pHeader
                P.many ((,) <$> pDate
                            <*> (catMaybes <$> pLogLine `P.manyTill` (void (P.lookAhead pDate) P.<|> P.eof)))

pHeader :: Parser String
pHeader = pWeekAgendaHeader
          P.<|> pDaysAgendaHeader
  where
    pWeekAgendaHeader = P.string "Week-agend" *> P.anyChar `P.manyTill` P.newline
    pDaysAgendaHeader = P.digit *> P.optional P.digit *> P.space *> P.string "days-agenda" *> P.anyChar `P.manyTill` P.newline

pLogLine :: Parser (Maybe LogLine)
pLogLine = do _ <- P.spaces *> P.anyChar `P.manyTill` P.space *> P.spaces
              P.try (Just <$> pClockedTask) P.<|> (Nothing <$ P.anyChar `P.manyTill` P.newline)

pClockedTask :: Parser LogLine
pClockedTask =
  do start <- pTime
     _ <- P.char '-' *> P.optional P.space
     end <- pTime
     _ <- P.space *> P.spaces *> P.string "Clocked:" *> P.spaces
          *> P.char '(' *> pTime *> P.char ')' *> P.spaces
     _ <- P.optional (pTaskState *> P.space)
     _ <- P.optional (pPriority)
     _ <- P.spaces
     (descr, tasks') <- pDescriptionAndTasksWithNewline
     return (LogLine start end descr tasks')

pDescriptionAndTasksWithNewline :: Parser (T.Text, [Task])
pDescriptionAndTasksWithNewline = go ""
  where
    go descr = do
      descr' <- T.pack <$> P.anyChar `P.manyTill` P.lookAhead (P.char ':')
      let combined = descr <> descr'
          cleaned = T.stripEnd combined
          pOnlyTasksLeft = (cleaned,) <$> pTasksFromTags <* (void P.newline P.<|> P.eof)
          pNoTasksInThisLine = const (cleaned, []) <$> (void P.newline P.<|> P.eof)
          pRecurse = do
            _ <- P.char ':'
            go (combined <> ":")
      P.try pOnlyTasksLeft P.<|> pNoTasksInThisLine P.<|> pRecurse


pTime :: Parser Time
pTime = do hour <- read <$> P.digit `P.manyTill` P.char ':'
           minute <- read <$> P.count 2 P.digit
           return (Time hour minute)

pTaskState :: Parser String
pTaskState = (P.try . P.choice . map P.string) ["TODO", "WAIT", "DONE", "CANC"]

pPriority :: Parser String
pPriority = P.try (P.string "[#" *> P.anyChar `P.manyTill` P.char ']')

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
pDay = read <$> P.many1 P.digit

pMonth :: Parser Int
pMonth = P.choice [  1 <$ P.try (P.string "January")
                  ,  2 <$ P.string "February"
                  ,  3 <$ P.try (P.string "March")
                  ,  4 <$ P.try (P.string "April")
                  ,  5 <$ P.string "May"
                  ,  6 <$ P.try (P.string "June")
                  ,  7 <$ P.string "July"
                  ,  8 <$ P.string "August"
                  ,  9 <$ P.string "September"
                  , 10 <$ P.string "October"
                  , 11 <$ P.string "November"
                  , 12 <$ P.string "December"
                  ]

pYear :: Parser Integer
pYear = read <$> P.count 4 P.digit

pWeekday :: Parser String
pWeekday = P.choice $
  map (P.try . P.string) [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"
                         , "Saturday", "Sunday"]

pTasksFromTags :: Parser [Task]
pTasksFromTags = P.many (P.try (pColon *> pTask)) <* P.many1 pColon

pTask :: Parser Task
pTask = T.pack <$> P.many1 (P.digit P.<|> P.lower P.<|> P.char '_')

pColon :: Parser Char
pColon = P.char ':'
