module Command where

import Control.Monad
import Data.Functor
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

data Command
  = CreatePoll
  | ShutUp
  | ScheduleHours [Int]
  | ScheduleDays [Int]
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
          , pScheduleDays
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
  guard $ (from > 0) && (to < 24) && (from < to)
  pure $ ScheduleHours [from, to]

pScheduleDays :: Parser Command
pScheduleDays = do
  string "schedule"
  days <- many1 (space *> pInt)
  guard (all (< 7) days)
  pure $ ScheduleDays days

pJoke :: Parser Command
pJoke = string "joke" $> TellJoke

pUnknown :: Parser Command
pUnknown = many1 anyChar $> UnknownCommand
