{-# LANGUAGE OverloadedStrings #-}
module Zeiterfassung.ParserSpec (spec) where

import Data.Time (fromGregorian)
import Test.Hspec

import qualified Text.Parsec as P

import Zeiterfassung.Parser

spec :: Spec
spec = do
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
