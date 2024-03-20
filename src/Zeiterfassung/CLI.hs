module Zeiterfassung.CLI where

import Options.Applicative
import System.Environment
import System.Exit
import Zeiterfassung.Redmine

data Command = ToRedmine
  deriving (Eq, Show)

mainParser :: ParserInfo Command
mainParser =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Do various things around time tracking"
    )

commandParser :: Parser Command
commandParser = subparser (command "to-redmine" (info (pure ToRedmine) (progDesc "Book times to redmine")))

getRedmineConfiguration :: IO RedmineConfig
getRedmineConfiguration =
  RedmineConfig
    <$> getFromEnv "REDMINE_URL" "the url of the Redmine instance"
    <*> getFromEnv "REDMINE_API_KEY" "the API key of the Redmine instance"
    <*> getFromEnv "REDMINE_USER_ID" "the user ID"

getFromEnv :: String -> String -> IO String
getFromEnv var help_ =
  lookupEnv var >>= \case
    Nothing -> die $ "Please set '" <> var <> "' to " <> help_
    Just value_ -> pure value_
