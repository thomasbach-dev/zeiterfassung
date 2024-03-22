module Zeiterfassung.RedmineSpec (spec) where

import Test.Hspec            (Spec, describe, it, runIO, shouldSatisfy)
import Zeiterfassung.CLI     (getRedmineConfiguration)
import Zeiterfassung.Redmine

spec :: Spec
spec = do
  describe "getTimeEntries" $ do
    redmineConfig <- runIO getRedmineConfiguration
    it "should return a non-empty list" $ do
      res <- getTimeEntries redmineConfig
      res `shouldSatisfy` not . null
