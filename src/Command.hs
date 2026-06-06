module Command where

import Data.Functor
import Data.Text
import Text.Parsec
import Text.Parsec.Text

data Command
  = CreatePoll
  | ShutUp
  | ScheduleHours [Int]
  | TellJoke
  | UnknownCommand
  deriving (Show)

getCmd :: Text -> Either ParseError Command
getCmd = parse pCommand "command"

pCommand :: Parser Command
pCommand =
  char '!'
    *> choice
      ( fmap
          try
          [ pPoll
          , pShutup
          , pScheduleHours
          , pJoke
          , pUnknown
          ]
      )

pPoll :: Parser Command
pPoll = string "poll" $> CreatePoll

pShutup :: Parser Command
pShutup = string "shutup" $> ShutUp

pInt :: Parser Int
pInt = read <$> many1 digit

pScheduleHours :: Parser Command
pScheduleHours = do
  string "schedule hours "
  from <- pInt
  space
  to <- pInt
  pure $ ScheduleHours [from, to]

pJoke :: Parser Command
pJoke = string "joke" $> TellJoke

pUnknown :: Parser Command
pUnknown = many1 anyChar $> UnknownCommand
