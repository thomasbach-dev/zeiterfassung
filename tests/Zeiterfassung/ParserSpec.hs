module Zeiterfassung.ParserSpec (spec) where

import           Data.Text                    (Text)
import           Data.Time
    (TimeOfDay (..), UTCTime (..), fromGregorian, timeOfDayToTime)
import           Test.Hspec                   (Spec, describe, it, shouldBe)
import qualified Text.Parsec                  as P
import           Zeiterfassung.Parser
import           Zeiterfassung.Representation

spec :: Spec
spec = do
  describe "pAgendaLog" $ do
    it "Parses an empty agenda log" $
      P.parse pAgendaLog "" example1 `shouldBe` Right []
    it "Parses a full agenda log example" $
      let expected =
            [ LogLine (timeOnFirstExampleDay 10 20) (timeOnFirstExampleDay 10 30) "Selbstorganisation" ["cons_q1_22"],
              LogLine (timeOnFirstExampleDay 11 25) (timeOnFirstExampleDay 12 40) "Fix failing tests in 3.2 Branch Tests" ["war"],
              LogLine (timeOnSndExampleDay 10 45) (timeOnSndExampleDay 11 05) "[INU-2697] Soap - Light2Full" ["cons_q1_22"]
            ]
          timeOnFirstExampleDay = mkExampleDay 2020 7 27
          timeOnSndExampleDay = mkExampleDay 2020 7 28
       in P.parse pAgendaLog "" example2 `shouldBe` Right expected
    it "Parses example 3" $
      let expected =
            [ LogLine (timeOnExampleDay 6 54) (timeOnExampleDay 7 05) "Configure VPN" ["bsb_iserv"],
              LogLine (timeOnExampleDay 8 50) (timeOnExampleDay 9 23) "Daily Orga and Stand Up" ["bsb_iserv"],
              LogLine (timeOnExampleDay 9 23) (timeOnExampleDay 11 12) "Project Intro" ["bsb_iserv"]
            ]
          timeOnExampleDay = mkExampleDay 2022 9 1
       in P.parse pAgendaLog "" example3 `shouldBe` Right expected

    it "Parses example 4" $
      let expected = [LogLine (timeOnExampleDay 11 01) (timeOnExampleDay 12 12) "TP1 -- Anforderungen zum Konnektor" ["dikom"]]
          timeOnExampleDay = mkExampleDay 2024 3 21
       in P.parse pAgendaLog "" example4 `shouldBe` Right expected

  describe "pDate" $ do
    it "Parses a date" $
      P.parse pDate "" "Tuesday    28 July 2020\n" `shouldBe` Right (fromGregorian 2020 7 28)
    it "Also parses the week number" $
      P.parse pDate "" "Monday     27 July 2020 W31\n" `shouldBe` Right (fromGregorian 2020 7 27)

  let exampleDay = fromGregorian 2020 7 27
      pLogLineWDay = pLogLine exampleDay
      timeOnExampleDay h m = UTCTime exampleDay (timeOfDayToTime $ TimeOfDay h m 0)
  describe "pLogLine" $ do
    it "Parses a log line" $
      P.parse pLogLineWDay "" "  gtd02:      10:20-10:30 Clocked:   (0:10) Zeiterfassung :cons_q1_22:\n"
        `shouldBe` (Right . Just) (LogLine (timeOnExampleDay 10 20) (timeOnExampleDay 10 30) "Zeiterfassung" ["cons_q1_22"])
    it "Handles tags from super-tasks correctly" $
      P.parse pLogLineWDay "" "  gtd02:      10:20-10:30 Clocked:   (0:10) Zeiterfassung :cons_q1_22::\n"
        `shouldBe` (Right . Just) (LogLine (timeOnExampleDay 10 20) (timeOnExampleDay 10 30) "Zeiterfassung" ["cons_q1_22"])
    it "Skips the task state" $
      P.parse pLogLineWDay "" "  gtd02:      10:50-12:55 Clocked:   (2:05) TODO [INU-2697] Soap - Light2Full :war:\n"
        `shouldBe` (Right . Just) (LogLine (timeOnExampleDay 10 50) (timeOnExampleDay 12 55) "[INU-2697] Soap - Light2Full" ["war"])
    it "Throws away an entry with an active clock" $
      P.parse pLogLineWDay "" "  gtd03:      15:00...... Daily/Weekly                    :cons_q1_22:\n"
        `shouldBe` Right Nothing
    it "parses log line with double space" $
      P.parse pLogLineWDay "" "  UNV:         6:54-7:05  Clocked:   (0:11) Configure VPN                    :bsb_iserv::\n"
        `shouldBe` (Right . Just) (LogLine (timeOnExampleDay 6 54) (timeOnExampleDay 7 05) "Configure VPN" ["bsb_iserv"])
    it "parses a log line with ticket number" $
      P.parse pLogLineWDay "" "  UNV:        15:11-15:25 Clocked:   (0:14) [36722] IServ-Connector: Matching  :bsb_iserv:\n"
        `shouldBe` (Right . Just) (LogLine (timeOnExampleDay 15 11) (timeOnExampleDay 15 25) "[36722] IServ-Connector: Matching" ["bsb_iserv"])
    it "parses a log line with missing tags" $
      P.parse pLogLineWDay "" "  UNV:        15:11-15:25 Clocked:   (0:14) Foo\n"
        `shouldBe` (Right . Just) (LogLine (timeOnExampleDay 15 11) (timeOnExampleDay 15 25) "Foo" [])

  describe "pTasksFromTags" $ do
    it "parses a single task" $
      P.parse pTasksFromTags "" ":cons_q1_22:"
        `shouldBe` Right ["cons_q1_22"]
    it "parses two tasks" $
      P.parse pTasksFromTags "" ":foo:bar:"
        `shouldBe` Right ["foo", "bar"]
    it "parses a task ended with two colons" $
      P.parse pTasksFromTags "" ":foo::"
        `shouldBe` Right ["foo"]
    it "parses a double colon in between" $
      P.parse pTasksFromTags "" ":foo::bar:"
        `shouldBe` Right ["foo", "bar"]

  describe "pClockedTask" $ do
    it "Parses " $
      P.parse (pClockedTask exampleDay) "" "15:11-15:25 Clocked:   (0:14) [36722] IServ-Connector: Matching  :bsb_iserv:\n"
        `shouldBe` Right (LogLine (timeOnExampleDay 15 11) (timeOnExampleDay 15 25) "[36722] IServ-Connector: Matching" ["bsb_iserv"])

mkExampleDay :: Integer -> Int -> Int -> (Int -> Int -> UTCTime)
mkExampleDay y m d = \h m' -> UTCTime day (timeOfDayToTime $ TimeOfDay h m' 0)
  where
    day = fromGregorian y m d

example1 :: Text
example1 =
  "Week-agenda (W31):\n\
  \Monday     27 July 2020 W31\n\
  \"

example2 :: Text
example2 =
  "Week-agenda (W31):\n\
  \Monday     27 July 2020 W31\n\
  \  gtd02:      10:20-10:30 Clocked:   (0:10) Selbstorganisation                         :cons_q1_22:\n\
  \  gtd02:      11:25-12:40 Clocked:   (1:15) TODO Fix failing tests in 3.2 Branch Tests :war:\n\
  \Tuesday    28 July 2020\n\
  \  gtd02:      10:45-11:05 Clocked:   (0:20) TODO [INU-2697] Soap - Light2Full          :cons_q1_22:\n\
  \Sunday      2 August 2020\n\
  \"

example3 :: Text
example3 =
  "15 days-agenda (W35-W37):\n\
  \Thursday    1 September 2022\n\
  \  UNV:         6:54-7:05  Clocked:   (0:11) Configure VPN                    :bsb_iserv:\n\
  \  UNV:         8:50-9:23  Clocked:   (0:33) Daily Orga and Stand Up          :bsb_iserv:\n\
  \  UNV:         9:00...... Daily Orga and Stand Up                            :bsb_iserv:\n\
  \  UNV:         9:23-11:12 Clocked:   (1:49) Project Intro                    :bsb_iserv:\n\
  \"

example4 :: Text
example4 =
  "Day-agenda (W12):\n\
  \Thursday   21 March 2024\n\
  \               8:00...... ----------------\n\
  \              10:00...... ----------------\n\
  \  bergwerk-it:11:01-12:12 Clocked:   (1:11) TP1 -- Anforderungen zum Konnektor :dikom::\n\
  \"
