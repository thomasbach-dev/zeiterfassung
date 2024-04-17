module Zeiterfassung.Redmine
  ( RedmineConfig (..),
    TimeEntry (..),
    IdWithName (..),
    PostTimeEntryRequest (..),
    logLineToTimeEntryCreate,
    GetTimeEntriesRequest (..),
    defaultGetTimeEntriesRequest,
    GetTimeEntriesResponse (..),
    getTimeEntries,
    postTimeEntry,
    deleteTimeEntry,
    getIssue,
    GetIssueResult (..),
    GetIssueResult'Issue (..),
    --
    ActivityId (..),
    IssueId (..),
    ProjectId (..),
    TimeEntryId (..),
    UserId (..),
  )
where

import           Control.Monad                (when)
import           Data.Aeson
    (FromJSON (parseJSON), ToJSON (..), Value (Object), defaultOptions, genericToEncoding, object,
    withObject, (.:), (.=))
import           Data.Aeson.Types             (Parser, prependFailure, typeMismatch)
import           Data.ByteString.Char8        (pack)
import qualified Data.ByteString.Char8        as BSC
import qualified Data.HashMap.Strict          as HM
import           Data.Maybe                   (catMaybes, mapMaybe)
import qualified Data.Text                    as T
import           Data.Time                    (Day, UTCTime (utctDay))
import           GHC.Generics                 (Generic)
import           Network.HTTP.Simple
    (Request, Response, getResponseBody, getResponseStatusCode, httpJSON, httpNoBody,
    setRequestBodyJSON, setRequestHeader, setRequestMethod, setRequestPath, setRequestQueryString)
import           System.Exit                  (die)
import           System.Log.Logger            (errorM)
import           Zeiterfassung.Representation

moduleLogger :: String
moduleLogger = "Zeiterfassung.Redmine"

-- * Configuration

data RedmineConfig = RedmineConfig
  { baseRequest :: !Request,
    apiKey      :: !String,
    userId      :: !UserId,
    projectMap  :: !ProjectMap
  }
  deriving (Show)

type ProjectMap = HM.HashMap Task ProjectDef

data ProjectDef = ProjectDef
  { issue_id    :: !IssueId,
    activity_id :: !ActivityId
  }
  deriving (Eq, Generic, Show)

instance FromJSON ProjectDef

-- * Time Entries

-- ** Create

logLineToTimeEntryCreate :: RedmineConfig -> LogLine -> IO PostTimeEntryRequest
logLineToTimeEntryCreate config logline = do
  projectDef <- case mapMaybe (`HM.lookup` config.projectMap) logline.tasks of
    []      -> die $ "Cannot find project definition for " <> show logline.tasks
    (x : _) -> pure x
  let issue_id = projectDef.issue_id
      activity_id = projectDef.activity_id
  pure PostTimeEntryRequest {..}
  where
    spent_on = Just . utctDay $ startTime logline
    hours = loggedHours logline
    comments = logline.subject
    user_id = userId config

postTimeEntry :: RedmineConfig -> PostTimeEntryRequest -> IO TimeEntry
postTimeEntry cfg entry = do
  resp <- performRequestJSON reqMod cfg
  when (getResponseStatusCode resp /= 201) $ do
    errorM loggerName $ "Error while posting time entry " <> show entry
    errorM loggerName $ show resp
    die "Exiting"
  pure . unWrappedTimeEntry . getResponseBody $ resp
  where
    reqMod =
      setRequestMethod "POST"
        . setRequestPath "/time_entries.json"
        . setRequestBodyJSON (WrappedTimeEntry entry)
    loggerName = moduleLogger <> ".postTimeEntry"

newtype WrappedTimeEntry a = WrappedTimeEntry
  { unWrappedTimeEntry :: a
  }
  deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (WrappedTimeEntry a) where
  toJSON :: WrappedTimeEntry a -> Value
  toJSON (WrappedTimeEntry entry) = object ["time_entry" .= entry]

instance (FromJSON a) => FromJSON (WrappedTimeEntry a) where
  parseJSON :: Value -> Parser (WrappedTimeEntry a)
  parseJSON = withObject "WrappedTimeEntry" $ \v -> WrappedTimeEntry <$> v .: "time_entry"

data PostTimeEntryRequest = PostTimeEntryRequest
  { -- | required The alternative is to specify the project id
    issue_id    :: !IssueId,
    -- | the date the time was spent (default to the current date)
    spent_on    :: !(Maybe Day),
    -- | (required): the number of spent hours
    hours       :: !Double,
    -- | the id of the time activity. This parameter is required unless a default activity is
    -- defined in Redmine.
    activity_id :: !ActivityId,
    -- | short description for the entry (255 characters max)
    comments    :: !T.Text,
    -- | user id to be specified in need of posting time on behalf of another user
    user_id     :: !UserId
  }
  deriving (Eq, Generic, Show)

instance ToJSON PostTimeEntryRequest where
  toEncoding = genericToEncoding defaultOptions

-- ** Listing

-- | Return a list of all time entries. This currently filters for the user id
-- from 'RedmineConfig'.
getTimeEntries :: RedmineConfig -> GetTimeEntriesRequest -> IO GetTimeEntriesResponse
getTimeEntries cfg req = do
  res <- performRequestJSON reqMod cfg
  when (getResponseStatusCode res /= 200) $ do
    errorM loggerName "Error on getting time entries"
    errorM loggerName $ show res
    die "Exiting"
  pure (getResponseBody res)
  where
    reqMod =
      setRequestPath "/time_entries.json"
        . setRequestQueryString
          ( catMaybes
              [ ("user_id",) . Just . BSC.pack . show . unUserId <$> req.user_id,
                ("project_id",) . Just . BSC.pack . show . unProjectId <$> req.project_id,
                ("limit",) . Just . BSC.pack . show <$> req.limit,
                ("from",) . Just . BSC.pack . show <$> req.from,
                ("to",) . Just . BSC.pack . show <$> req.to
              ]
          )
    loggerName = moduleLogger <> ".getTimeEntries"

defaultGetTimeEntriesRequest :: GetTimeEntriesRequest
defaultGetTimeEntriesRequest = GetTimeEntriesRequest Nothing Nothing Nothing Nothing Nothing

data GetTimeEntriesRequest = GetTimeEntriesRequest
  { project_id :: Maybe ProjectId,
    user_id    :: Maybe UserId,
    limit      :: Maybe Int,
    from       :: Maybe Day,
    to         :: Maybe Day
  }
  deriving (Eq, Show)

data GetTimeEntriesResponse = GetTimeEntriesResponse
  { limit        :: !Int,
    offset       :: !Int,
    time_entries :: ![TimeEntry],
    total_count  :: !Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON GetTimeEntriesResponse

newtype WrappedId = WrappedId {unWrappedId :: Int} deriving (Eq, Show)

instance FromJSON WrappedId where
  parseJSON (Object v) = WrappedId <$> v .: "id"
  parseJSON invalid    = prependFailure "parsing WrappedId failed, " $ typeMismatch "Object" invalid

-- ** Delete

deleteTimeEntry :: RedmineConfig -> TimeEntryId -> IO ()
deleteTimeEntry cfg id_ = do
  res <- httpNoBody request
  when (getResponseStatusCode res /= 200) $ do
    errorM loggerName $ "Error while deleting time entry " <> show id_
    errorM loggerName $ show res
    die "Exiting"
  where
    request =
      setRequestMethod "DELETE"
        . setRequestPath ("/time_entries/" <> (BSC.pack . show) id_ <> ".json")
        . buildRequest
        $ cfg
    loggerName = moduleLogger <> ".deleteTimeEntry"

-- * Issues

getIssue :: IssueId -> RedmineConfig -> IO (Response GetIssueResult)
getIssue issueId =
  performRequestJSON
    (setRequestPath ("/issues/" <> (BSC.pack . show) issueId <> ".json"))

newtype GetIssueResult = GetIssueResult
  { issue :: GetIssueResult'Issue
  }
  deriving (Eq, Generic, Show)

instance FromJSON GetIssueResult

data GetIssueResult'Issue = GetIssueResult'Issue
  { assigned_to       :: !(IdWithName UserId),
    author            :: !(IdWithName UserId),
    created_on        :: !UTCTime,
    css_classes       :: !T.Text,
    custom_fields     :: ![CustomFields],
    description       :: !T.Text,
    done_ratio        :: !Double,
    due_date          :: !Day,
    id                :: !IssueId,
    priority          :: !IssuePriority,
    project           :: !(IdWithName ProjectId),
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

data TimeEntry = TimeEntry
  { activity         :: !(IdWithName ActivityId),
    comments         :: !T.Text,
    created_on       :: !UTCTime,
    custom_fields    :: ![CustomFields],
    easy_is_billable :: !Bool,
    entity_id        :: !Int,
    entity_type      :: !T.Text,
    hours            :: !Double,
    id               :: !TimeEntryId,
    issue            :: !(Maybe WrappedId),
    project          :: !(IdWithName ProjectId),
    spent_on         :: !Day,
    updated_on       :: !UTCTime,
    user             :: !(IdWithName UserId)
  }
  deriving (Eq, Show, Generic)

instance FromJSON TimeEntry

data IdWithName a = IdWithName
  { id   :: !a,
    name :: !T.Text
  }
  deriving (Eq, Generic, Show)

instance (FromJSON a) => FromJSON (IdWithName a)

newtype ActivityId = ActivityId
  { unActivityId :: Int
  }
  deriving (Eq)
  deriving (Read, Show, FromJSON, ToJSON) via Int

newtype IssueId = IssueId
  { unIssueId :: Int
  }
  deriving (Eq)
  deriving (Read, Show, FromJSON, ToJSON) via Int

newtype ProjectId = ProjectId
  { unProjectId :: Int
  }
  deriving (Eq)
  deriving (Read, Show, FromJSON, ToJSON) via Int

newtype TimeEntryId = TimeEntryId
  { unTimeEntryId :: Int
  }
  deriving (Eq)
  deriving (Read, Show, FromJSON) via Int

newtype UserId = UserId {unUserId :: Int}
  deriving (Eq)
  deriving (Read, Show, FromJSON, ToJSON) via Int

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

performRequestJSON :: (FromJSON a) => (Request -> Request) -> RedmineConfig -> IO (Response a)
performRequestJSON reqMod = httpJSON . reqMod . buildRequest

buildRequest :: RedmineConfig -> Request
buildRequest RedmineConfig {..} = setRequestHeader "X-Redmine-API-Key" [pack apiKey] baseRequest
