module Zeiterfassung.CLI
  ( cliMain,
    getRedmineConfiguration,
  )
where

import Control.Monad                (forM_, when)
import Data.Aeson                   (eitherDecodeFileStrict)
import Data.Time                    (timeToDaysAndTimeOfDay)
import Network.HTTP.Simple          (parseRequest)
import Options.Applicative
    (Parser, ParserInfo, argument, auto, command, execParser, fullDesc, help, helper, info, long,
    metavar, option, progDesc, short, showDefault, str, subparser, switch, value, (<**>))
import System.Environment           (lookupEnv)
import System.Exit                  (die)
import System.Log.Logger
    (Priority (INFO), debugM, infoM, rootLoggerName, setLevel, updateGlobalLogger)
import Zeiterfassung.Parser
import Zeiterfassung.Redmine
import Zeiterfassung.Representation

moduleLogger :: String
moduleLogger = "Zeiterfassung.CLI"

data MainArgs = MainArgs
  { logLevel    :: Priority,
    mainCommand :: MainCommand
  }
  deriving (Eq, Show)

data MainCommand = ToRedmine ToRedmineArgs
  deriving (Eq, Show)

cliMain :: IO ()
cliMain = do
  args <- execParser mainParser
  updateGlobalLogger rootLoggerName (setLevel $ args.logLevel)
  debugM "CLI.cliMain" $ "Parsed command line args: " <> show args
  case args.mainCommand of
    ToRedmine redmineArgs -> toRedmineMain redmineArgs

mainParser :: ParserInfo MainArgs
mainParser =
  info
    (mainArgsParser <**> helper)
    ( fullDesc
        <> progDesc "Do various things around time tracking"
    )
  where
    mainArgsParser =
      MainArgs
        <$> logLevelParser
        <*> mainCommandParser
    logLevelParser =
      option
        auto
        ( long "log-level"
            <> help "Set the log level (DEBUG, INFO, WARNING, ERROR, CRITICAL)"
            <> short 'l'
            <> metavar "LEVEL"
            <> showDefault
            <> Options.Applicative.value INFO
        )

mainCommandParser :: Parser MainCommand
mainCommandParser =
  subparser
    ( command
        "to-redmine"
        (info (ToRedmine <$> redmineParser <**> helper) (progDesc "Book times to redmine"))
    )

-- * Publish to Redmine

toRedmineMain :: ToRedmineArgs -> IO ()
toRedmineMain args = do
  cfg <- getRedmineConfiguration
  loglines <- readAgendaFile args.agendaFile
  mapM_ (debugM loggerName) $
    "Read log lines:" : map show loglines
  allEntries <- mapM (logLineToTimeEntryCreate cfg) loglines
  mapM_ (debugM loggerName) $
    "Mapped to the following time entries:" : map show allEntries

  let totalSpent :: Double = sum . map (\x -> x.hours) $ entries
      actuallySpent = sum . map loggedTime $ loglines
      entries = filter (\x -> x.hours /= 0) allEntries
  infoM loggerName $ "Create time entries with a total spent hours of: " <> show totalSpent
  infoM loggerName $ "Actually spent: " <> (show . timeToDaysAndTimeOfDay) actuallySpent
  when args.dryRun $ do
    die "Dry run mode! Exiting"
  forM_ entries $ \entry -> do
    infoM loggerName $ "Creating " <> show entry
    resp <- postTimeEntry cfg entry
    debugM loggerName $ "Result: " <> show resp
  where
    loggerName = moduleLogger <> ".toRedmineMain"

data ToRedmineArgs = ToRedmineArgs
  { dryRun     :: Bool,
    agendaFile :: String
  }
  deriving (Eq, Show)

redmineParser :: Parser ToRedmineArgs
redmineParser =
  ToRedmineArgs
    <$> switch (long "dry-run" <> short 'n' <> help "Do not create any time entry")
    <*> argument str (metavar "FILE" <> help "The agenda file to process")

getRedmineConfiguration :: IO RedmineConfig
getRedmineConfiguration = do
  baseRequest <- parseRequest =<< getFromEnv "REDMINE_URL" "the url of the Redmine instance"
  apiKey <- getFromEnv "REDMINE_API_KEY" "the API key of the Redmine instance"
  userId <- read <$> getFromEnv "REDMINE_USER_ID" "the user ID"
  projectMapFile <- getFromEnv "REDMINE_PROJECT_MAP" "the Redmine project map"
  projectMap <-
    eitherDecodeFileStrict projectMapFile >>= \case
      Left err -> die $ "Could not read project map file: " <> err
      Right m -> pure m
  pure RedmineConfig {..}

getFromEnv :: String -> String -> IO String
getFromEnv var help_ =
  lookupEnv var >>= \case
    Nothing -> die $ "Please set '" <> var <> "' to " <> help_
    Just value_ -> pure value_
