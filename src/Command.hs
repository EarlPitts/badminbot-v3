module Command where

import Data.Functor
import Data.Text
import Text.Parsec
import Text.Parsec.Text

data Command
  = CreatePoll
  | ShutUp
  | TellJoke
  | UnknownCommand
  deriving (Show)

getCmd :: Text -> Either ParseError Command
getCmd = parse pCommand "command"

pCommand :: Parser Command
pCommand = char '!' *> choice [pPoll, pShutup, pJoke, pUnknown]

pPoll :: Parser Command
pPoll = string "poll" $> CreatePoll

pShutup :: Parser Command
pShutup = string "shutup" $> ShutUp

pJoke :: Parser Command
pJoke = string "joke" $> TellJoke

pUnknown :: Parser Command
pUnknown = many1 anyChar $> UnknownCommand
