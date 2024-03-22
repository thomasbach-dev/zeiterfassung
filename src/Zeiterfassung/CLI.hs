module Zeiterfassung.CLI where

import Data.Aeson            (eitherDecodeFileStrict)
import Network.HTTP.Simple   (parseRequest)
import Options.Applicative
    (Parser, ParserInfo, argument, auto, command, execParser, fullDesc, help, helper, info, long,
    metavar, option, progDesc, short, showDefault, str, subparser, value, (<**>))
import System.Environment    (lookupEnv)
import System.Exit           (die)
import System.Log.Logger
import Zeiterfassung.Parser
import Zeiterfassung.Redmine

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
  mapM_ (debugM "CLI.toRedmineMain") ("Read log lines:" : map show loglines)

data ToRedmineArgs = ToRedmineArgs
  { agendaFile :: String
  }
  deriving (Eq, Show)

redmineParser :: Parser ToRedmineArgs
redmineParser = ToRedmineArgs <$> argument str (metavar "FILE" <> help "The agenda file to process")

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
