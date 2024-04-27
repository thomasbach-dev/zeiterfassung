module Zeiterfassung.RedmineSpec (spec) where

import Control.Exception     (IOException)
import Control.Monad.Catch   (try)
import Test.Hspec            (Spec, describe, it, runIO, shouldSatisfy, xit)
import Zeiterfassung.CLI     (getRedmineConfiguration)
import Zeiterfassung.Redmine

spec :: Spec
spec = do
  describe "getTimeEntries" $ do
    triedRedmineConfig <- runIO (try getRedmineConfiguration)
    let runIfConfigured = case triedRedmineConfig of
          Left (_ :: IOException) -> \descr f -> xit descr (f undefined)
          Right cfg               -> \descr f -> it descr (f cfg)
    runIfConfigured "should return a non-empty list" $ \redmineConfig -> do
      res <- getTimeEntries redmineConfig (defaultGetTimeEntriesRequest {limit = Just 1})
      res `shouldSatisfy` ((== 1) . length . time_entries)
