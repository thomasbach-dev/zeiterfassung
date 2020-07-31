module Zeiterfassung where

import Control.Applicative ((<$))
import Data.Time (Day, fromGregorian)
import Text.Parsec.Text (Parser)

import qualified Text.Parsec as P

pWeekday :: Parser String
pWeekday = P.choice $
  map P.string [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"
               , "Saturday", "Sunday"]

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
