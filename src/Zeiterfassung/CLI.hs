module Zeiterfassung.CLI (mainParser) where

import Options.Applicative

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
