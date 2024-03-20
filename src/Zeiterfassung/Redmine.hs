{-# LANGUAGE InstanceSigs #-}

module Zeiterfassung.Redmine where

import           Data.Aeson            (FromJSON (parseJSON), Value (Object), (.:))
import           Data.Aeson.Types      (prependFailure, typeMismatch)
import           Data.ByteString.Char8 (pack)
import qualified Data.Text             as T
import           Data.Time             (Day, UTCTime)
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple   (Response, httpJSON, parseRequest, setRequestHeader)

data RedmineConfig = RedmineConfig
  { url    :: !String,
    apiKey :: !String,
    userId :: !String
  }
  deriving (Eq, Show)

data TimeEntriesResult = TimeEntriesResult
  { limit        :: !Int,
    offset       :: !Int,
    time_entries :: ![TimeEntry],
    total_count  :: !Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON TimeEntriesResult

data TimeEntry = TimeEntry
  { activity         :: !Activity,
    comments         :: !T.Text,
    created_on       :: !UTCTime,
    custom_fields    :: ![CustomFields],
    easy_is_billable :: !Bool,
    entity_id        :: !Int,
    entity_type      :: !T.Text,
    hours            :: !Double,
    id               :: !Int,
    issue            :: !(Maybe WrappedId),
    project          :: !Project,
    spent_on         :: !Day,
    updated_on       :: !UTCTime,
    user             :: !User
  }
  deriving (Eq, Show, Generic)

instance FromJSON TimeEntry

data Activity = Activity
  { id   :: !Int,
    name :: !T.Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Activity

data CustomFields = CustomFields
  { field_format  :: !T.Text,
    id            :: !Int,
    internal_name :: !(Maybe T.Text),
    name          :: !T.Text,
    value         :: !(Maybe T.Text)
  }
  deriving (Eq, Show, Generic)

instance FromJSON CustomFields

newtype WrappedId = WrappedId {unWrappedId :: Int} deriving (Eq, Show)

instance FromJSON WrappedId where
  parseJSON (Object v) = WrappedId <$> v .: "id"
  parseJSON invalid    = prependFailure "parsing WrappedId failed, " $ typeMismatch "Object" invalid

data Project = Project
  { id   :: !Int,
    name :: !T.Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Project

data User = User
  { id   :: !Int,
    name :: !T.Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON User

listTimeEntries :: RedmineConfig -> IO (Response TimeEntriesResult)
listTimeEntries RedmineConfig {..} = do
  request <-
    setRequestHeader "X-Redmine-API-Key" [pack apiKey]
      <$> parseRequest requestUrl
  httpJSON request
  where
    requestUrl = url <> "/time_entries.json?user_id=" <> userId
