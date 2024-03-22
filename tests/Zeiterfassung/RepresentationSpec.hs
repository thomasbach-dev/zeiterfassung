module Zeiterfassung.RepresentationSpec (spec) where

import Data.Time
    (TimeOfDay (..), UTCTime (..), addUTCTime, fromGregorian, midday, midnight, timeOfDayToTime)
import Test.Hspec                   (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck        (prop)
import Test.QuickCheck              ((==>))
import Test.QuickCheck.Instances    ()
import Zeiterfassung.Representation

spec :: Spec
spec = do
  describe "todRoundToNextNMinutes" $ do
    prop "result can be divided by n" $ \n t ->
      0 < n
        && n
          < 60 ==> do
            let (TimeOfDay _ m _) = todRoundToNextNMinutes n t
            (m `mod` n) `shouldBe` 0

  describe "todRoundToNextFiveMinutes" $ do
    it "Returns identity on 0s, 5s and 10s" $
      map todRoundToNextFiveMinutes [minutesToTOD 0, minutesToTOD 15, minutesToTOD 20]
        `shouldBe` [minutesToTOD 0, minutesToTOD 15, minutesToTOD 20]
    it "Floors on 1s, 2s, 6s and 7s" $
      map todRoundToNextFiveMinutes [minutesToTOD 1, minutesToTOD 12, minutesToTOD 26, minutesToTOD 37]
        `shouldBe` [minutesToTOD 0, minutesToTOD 10, minutesToTOD 25, minutesToTOD 35]
    it "Ceils on 3s, 4s, 8s and 9s" $
      map todRoundToNextFiveMinutes [minutesToTOD 3, minutesToTOD 14, minutesToTOD 28, minutesToTOD 39]
        `shouldBe` [minutesToTOD 5, minutesToTOD 15, minutesToTOD 30, minutesToTOD 40]
    it "Also rounds up to the next hour correctly" $
      todRoundToNextFiveMinutes (minutesToTOD 58) `shouldBe` TimeOfDay 1 0 0

  describe "loggedhours" $ do
    let example1 = LogLine timestamp1 timestamp2 "" []
        timestamp1 = UTCTime aDay (timeOfDayToTime midnight)
        timestamp2 = UTCTime aDay (timeOfDayToTime midday)
        aDay = fromGregorian 2024 1 1
    it "determines correctly full hours" $
      loggedHours example1 `shouldBe` 12.0
    it "deterimens correctly half hours" $
      let example3 = LogLine timestamp1 timestamp3 "" []
          timestamp3 = ((30 * 60) `addUTCTime` timestamp1)
       in loggedHours example3 `shouldBe` 0.5

minutesToTOD :: Int -> TimeOfDay
minutesToTOD m = TimeOfDay 0 m 0
