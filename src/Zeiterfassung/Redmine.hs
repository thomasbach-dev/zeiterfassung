module Zeiterfassung.Redmine where

import           Data.Aeson
    (FromJSON (parseJSON), ToJSON (..), Value (Object), defaultOptions, genericToEncoding, object,
    (.:), (.=))
import           Data.Aeson.Types             (prependFailure, typeMismatch)
import           Data.ByteString.Char8        (pack)
import qualified Data.ByteString.Char8        as BSC
import qualified Data.HashMap.Strict          as HM
import           Data.Maybe                   (mapMaybe)
import qualified Data.Text                    as T
import           Data.Time                    (Day, UTCTime (utctDay))
import           GHC.Generics                 (Generic)
import           Network.HTTP.Simple
    (Request, Response, httpJSON, setRequestHeader, setRequestPath, setRequestQueryString)
import           Zeiterfassung.Representation

-- * Configuration

data RedmineConfig = RedmineConfig
  { baseRequest :: !Request,
    apiKey      :: !String,
    userId      :: !Int
  }
  deriving (Show)

type ProjectMap = HM.HashMap Task ProjectDef

data ProjectDef = ProjectDef
  { issue_id    :: !Int,
    activity_id :: !Int
  }
  deriving (Eq, Show)

-- * Time Entries

-- ** Create

logLineToTimeEntryCreate :: RedmineConfig -> ProjectMap -> LogLine -> IO TimeEntryCreate
logLineToTimeEntryCreate config projectMap logline = do
  projectDef <- case mapMaybe (`HM.lookup` projectMap) logline.tasks of
    []      -> error $ "Cannot find project definition for " <> show logline.tasks
    (x : _) -> pure x
  let issue_id = projectDef.issue_id
      activity_id = projectDef.activity_id
  pure TimeEntryCreate {..}
  where
    spent_on = Just . utctDay $ startTime logline
    hours = loggedHours logline
    comments = logline.subject
    user_id = userId config

newtype ActivityId = ActivityId
  { unActivityId :: Int
  }
  deriving (Eq, Show)
  deriving (FromJSON) via Int

newtype WrappedTimeEntryCreate = WrappedTimeEntryCreate
  { unWrappedTimeEntryCreate :: TimeEntryCreate
  }
  deriving (Eq, Show)

instance ToJSON WrappedTimeEntryCreate where
  toJSON (WrappedTimeEntryCreate entry) = object ["time_entry" .= entry]

data TimeEntryCreate = TimeEntryCreate
  { -- | required The alternative is to specify the project id
    issue_id    :: !Int,
    -- | the date the time was spent (default to the current date)
    spent_on    :: !(Maybe Day),
    -- | (required): the number of spent hours
    hours       :: !Double,
    -- | the id of the time activity. This parameter is required unless a default activity is
    -- defined in Redmine.
    activity_id :: !Int,
    -- | short description for the entry (255 characters max)
    comments    :: !T.Text,
    -- | user id to be specified in need of posting time on behalf of another user
    user_id     :: !Int
  }
  deriving (Eq, Generic, Show)

instance ToJSON TimeEntryCreate where
  toEncoding = genericToEncoding defaultOptions

-- ** Listing

-- | Return a list of all time entries. This currently filters for the user id
-- from 'RedmineConfig'.
getTimeEntries :: RedmineConfig -> IO (Response GetTimeEntriesResponse)
getTimeEntries cfg@RedmineConfig {..} = performRequest reqMod cfg
  where
    reqMod = setRequestPath "/time_entries.json" . setRequestQueryString [("user_id", (Just . BSC.pack . show) userId)]

data GetTimeEntriesResponse = GetTimeEntriesResponse
  { limit        :: !Int,
    offset       :: !Int,
    time_entries :: ![GetTimeEntriesResponse'TimeEntry],
    total_count  :: !Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON GetTimeEntriesResponse

data GetTimeEntriesResponse'TimeEntry = GetTimeEntriesResponse'TimeEntry
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

instance FromJSON GetTimeEntriesResponse'TimeEntry

data Activity = Activity
  { id   :: !Int,
    name :: !T.Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Activity

newtype WrappedId = WrappedId {unWrappedId :: Int} deriving (Eq, Show)

instance FromJSON WrappedId where
  parseJSON (Object v) = WrappedId <$> v .: "id"
  parseJSON invalid    = prependFailure "parsing WrappedId failed, " $ typeMismatch "Object" invalid

-- * Issues

getIssue :: IssueId -> RedmineConfig -> IO (Response GetIssueResult)
getIssue issueId =
  performRequest
    (setRequestPath ("/issues/" <> (BSC.pack . show . unIssueId) issueId <> ".json"))

newtype IssueId = IssueId {unIssueId :: Int}
  deriving (FromJSON) via Int
  deriving (Eq, Show)

newtype GetIssueResult = GetIssueResult
  { issue :: GetIssueResult'Issue
  }
  deriving (Eq, Generic, Show)

instance FromJSON GetIssueResult

data GetIssueResult'Issue = GetIssueResult'Issue
  { assigned_to       :: !User,
    author            :: !User,
    created_on        :: !UTCTime,
    css_classes       :: !T.Text,
    custom_fields     :: ![CustomFields],
    description       :: !T.Text,
    done_ratio        :: !Double,
    due_date          :: !Day,
    id                :: !IssueId,
    priority          :: !IssuePriority,
    project           :: !Project,
    spent_hours       :: !Double,
    start_date        :: !Day,
    status            :: !IssueStatus,
    subject           :: !T.Text,
    total_spent_hours :: !Double,
    tracker           :: !IssueTracker,
    updated_on        :: !UTCTime
  }
  deriving (Eq, Generic, Show)

instance FromJSON GetIssueResult'Issue

data IssuePriority = IssuePriority
  { id   :: !Int,
    name :: !T.Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON IssuePriority

data IssueStatus = IssueStatus
  { id   :: !Int,
    name :: !T.Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON IssueStatus

data IssueTracker = IssueTracker
  { id   :: !Int,
    name :: !T.Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON IssueTracker

-- * Common records used in several requests

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

data CustomFields = CustomFields
  { field_format  :: !T.Text,
    id            :: !Int,
    internal_name :: !(Maybe T.Text),
    name          :: !T.Text,
    value         :: !(Maybe T.Text)
  }
  deriving (Eq, Show, Generic)

instance FromJSON CustomFields

-- * Utils

performRequest :: (FromJSON a) => (Request -> Request) -> RedmineConfig -> IO (Response a)
performRequest reqMod RedmineConfig {..} = httpJSON request
  where
    request = reqMod . setRequestHeader "X-Redmine-API-Key" [pack apiKey] $ baseRequest
