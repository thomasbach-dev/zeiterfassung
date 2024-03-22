module Zeiterfassung.CLI where

import Data.Aeson            (eitherDecodeFileStrict)
import Network.HTTP.Simple   (parseRequest)
import Options.Applicative
    (Parser, ParserInfo, argument, command, execParser, fullDesc, help, helper, info, metavar,
    progDesc, str, subparser, (<**>))
import System.Environment    (lookupEnv)
import System.Exit           (die)
import Zeiterfassung.Redmine

data Command = ToRedmine ToRedmineArgs
  deriving (Eq, Show)

cliMain :: IO ()
cliMain =
  execParser mainParser >>= \case
    ToRedmine args -> toRedmineMain args

mainParser :: ParserInfo Command
mainParser =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Do various things around time tracking"
    )

commandParser :: Parser Command
commandParser = subparser (command "to-redmine" (info (ToRedmine <$> redmineParser <**> helper) (progDesc "Book times to redmine")))

-- * Publish to Redmine

toRedmineMain :: ToRedmineArgs -> IO ()
toRedmineMain args = do
  print args

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
