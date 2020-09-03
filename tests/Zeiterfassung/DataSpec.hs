module Zeiterfassung.DataSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Zeiterfassung.Data

spec :: Spec
spec = do
  describe "roundToNextFiveMinutes" $ do
    it "Returns identity on 0s, 5s and 10s" $
      map roundToNextFiveMinutes [Time 0 0, Time 0 15, Time 0 20] 
        `shouldBe`               [Time 0 0, Time 0 15, Time 0 20]
    it "Floors on 1s, 2s, 6s and 7s" $
      map roundToNextFiveMinutes [Time 0 1, Time 0 12, Time 0 26, Time 0 37] 
        `shouldBe`               [Time 0 0, Time 0 10, Time 0 25, Time 0 35]
    it "Ceils on 3s, 4s, 8s and 9s" $
      map roundToNextFiveMinutes [Time 0 3, Time 0 14, Time 0 28, Time 0 39] 
        `shouldBe`               [Time 0 5, Time 0 15, Time 0 30, Time 0 40]
    it "Also rounds up to the next hour correctly" $
      roundToNextFiveMinutes (Time 1 58) `shouldBe` Time 2 0
