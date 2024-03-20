module Main
  ( main,
    spec,
  )
where

import           Test.Hspec                       (Spec, describe, hspec)
import qualified Zeiterfassung.ParserSpec
import qualified Zeiterfassung.RedmineSpec
import qualified Zeiterfassung.RepresentationSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Zeiterfassung.Parser" Zeiterfassung.ParserSpec.spec
  describe "Zeiterfassung.Redmine" Zeiterfassung.RedmineSpec.spec
  describe "Zeiterfassung.Representation" Zeiterfassung.RepresentationSpec.spec
