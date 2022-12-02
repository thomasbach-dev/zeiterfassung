module Zeiterfassung.RepresentationSpec (spec) where

import Data.Time  (TimeOfDay (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Zeiterfassung.Representation

spec :: Spec
spec = do
  describe "todRoundToNextFiveMinutes" $ do
    it "Returns identity on 0s, 5s and 10s" $
      map todRoundToNextFiveMinutes [minutesToTOD 0, minutesToTOD 15, minutesToTOD 20]
        `shouldBe`               [minutesToTOD 0, minutesToTOD 15, minutesToTOD 20]
    it "Floors on 1s, 2s, 6s and 7s" $
      map todRoundToNextFiveMinutes [minutesToTOD 1, minutesToTOD 12, minutesToTOD 26, minutesToTOD 37]
        `shouldBe`               [minutesToTOD 0, minutesToTOD 10, minutesToTOD 25, minutesToTOD 35]
    it "Ceils on 3s, 4s, 8s and 9s" $
      map todRoundToNextFiveMinutes [minutesToTOD 3, minutesToTOD 14, minutesToTOD 28, minutesToTOD 39]
        `shouldBe`               [minutesToTOD 5, minutesToTOD 15, minutesToTOD 30, minutesToTOD 40]
    it "Also rounds up to the next hour correctly" $
      todRoundToNextFiveMinutes (minutesToTOD 58) `shouldBe` TimeOfDay 1 0 0

minutesToTOD :: Int -> TimeOfDay
minutesToTOD m = TimeOfDay 0 m 0
