module Command where

import Data.Functor
import Data.Text
import Text.Parsec
import Text.Parsec.String

data Command = CreatePoll deriving (Show)

getCmd :: Text -> Either ParseError Command
getCmd msg = parse pCommand "command" (unpack msg)

pCommand :: Parser Command
pCommand = char '!' *> choice [pPoll]

pPoll :: Parser Command
pPoll = string "poll" $> CreatePoll
