module Zeiterfassung.Config
  ( Config
  , AccountMap
  , readConfig
  ) where

import qualified Data.Aeson as A
import qualified Data.Map   as M
import qualified Data.Text  as T

import System.Exit (die)

type Config = AccountMap
type AccountMap = M.Map T.Text T.Text

readConfig :: FilePath -> IO AccountMap
readConfig filePath = do
  eitherDecoded <- A.eitherDecodeFileStrict filePath
  case eitherDecoded of
    Left err      -> die err
    Right decoded -> pure decoded
