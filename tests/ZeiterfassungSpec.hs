{-# LANGUAGE OverloadedStrings #-}
module ZeiterfassungSpec (spec) where

import Data.Time (fromGregorian)
import Test.Hspec

import qualified Text.Parsec as P

import Zeiterfassung

spec :: Spec
spec = do
  describe "pDate" $ do
    it "Parses a date" $
      P.parse pDate "" "Tuesday    28 July 2020\n" `shouldBe` Right (fromGregorian 2020 7 28)
    it "Also parses the week number" $
      P.parse pDate "" "Monday     27 July 2020 W31\n" `shouldBe` Right (fromGregorian 2020 7 27)
