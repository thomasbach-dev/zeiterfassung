{-# LANGUAGE OverloadedStrings #-}
module Zeiterfassung.ParserSpec (spec) where

import Data.Time (fromGregorian)
import Data.Text (Text)
import Test.Hspec

import qualified Text.Parsec as P

import Zeiterfassung.Data
import Zeiterfassung.Parser

spec :: Spec
spec = do
  describe "pAgendaLog" $ do
    it "Parses an empty agenda log" $
      P.parse pAgendaLog "" example1 `shouldBe` Right [(fromGregorian 2020 7 27, [])]
    it "Parses a full agenda log example" $
      let expected = 
            [(fromGregorian 2020 7 27, [ LogLine (Time 10 20) (Time 10 30) "Selbstorganisation"
                                       , LogLine (Time 11 25) (Time 12 40) "Fix failing tests in 3.2 Branch Tests"
                                       ])
            ,(fromGregorian 2020 7 28, [ LogLine (Time 10 45) (Time 11 05) "[INU-2697] CR204/SOAP-3: Soap - Light2Full"])
            ,(fromGregorian 2020 8  2, [])
            ]
      in P.parse pAgendaLog "" example2 `shouldBe` Right expected
  describe "pDate" $ do
    it "Parses a date" $
      P.parse pDate "" "Tuesday    28 July 2020\n" `shouldBe` Right (fromGregorian 2020 7 28)
    it "Also parses the week number" $
      P.parse pDate "" "Monday     27 July 2020 W31\n" `shouldBe` Right (fromGregorian 2020 7 27)
  describe "pLogLine" $ do
    it "Parses a log line" $
      P.parse pLogLine "" "  gtd02:      10:20-10:30 Clocked:   (0:10) Selbstorganisation\n"
        `shouldBe` Right (LogLine (Time 10 20) (Time 10 30) "Selbstorganisation")
    it "Skips the task state" $
      P.parse pLogLine "" "  gtd02:      10:50-12:55 Clocked:   (2:05) TODO [INU-2697] CR204/SOAP-3: Soap - Light2Full\n"
        `shouldBe` Right (LogLine (Time 10 50) (Time 12 55) "[INU-2697] CR204/SOAP-3: Soap - Light2Full")

example1 :: Text
example1 =
  "Week-agenda (W31):\n\
   \Monday     27 July 2020 W31\n\
   \"

example2 :: Text
example2 =
  "Week-agenda (W31):\n\
   \Monday     27 July 2020 W31\n\
   \  gtd02:      10:20-10:30 Clocked:   (0:10) Selbstorganisation\n\
   \  gtd02:      11:25-12:40 Clocked:   (1:15) TODO Fix failing tests in 3.2 Branch Tests\n\
   \Tuesday    28 July 2020\n\
   \  gtd02:      10:45-11:05 Clocked:   (0:20) TODO [INU-2697] CR204/SOAP-3: Soap - Light2Full\n\
   \Sunday      2 August 2020\n\
   \"
