{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot (runBot) where

import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Data.Text (Text, isPrefixOf, lines, pack, toLower)
import qualified Data.Text.IO as TIO
import System.Environment
import System.Random (randomRIO)
import UnliftIO.Concurrent
import Prelude hiding (lines)

import Discord
import qualified Discord.Requests as R
import Discord.Types

import Command
import qualified Poll as P

data Env = Env
  { -- config :: Config
    jokes :: [Text]
  }
  deriving (Show)

-- data Config = Config
--   { days :: [Int]
--   , slots :: [Int]
--   }
--   deriving (Show)

runBot :: IO ()
runBot = do
  token <- pack <$> getEnv "TOKEN"
  badminbot token

badminbot :: Text -> IO ()
badminbot token = do
  putStrLn "started..."
  js <- lines <$> TIO.readFile "jokes.txt"
  let env = Env js
  userFacingError <-
    runDiscord $
      def
        { discordToken = "Bot " <> token
        , discordOnEvent = eventHandler env
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }
  TIO.putStrLn userFacingError

-- userFacingError is an unrecoverable error
-- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Env -> Event -> DiscordHandler ()
eventHandler env event = case event of
  MessageCreate msg -> handleMessage env msg
  -- MessageCreate m -> when (isPing m && not (fromBot m)) $ do
  --   void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
  --   threadDelay (2 * 10 ^ 6)
  --   void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
  _ -> return ()

handleMessage :: Env -> Message -> DiscordHandler ()
handleMessage Env{..} msg = unless (fromBot msg) $ do
  let cmd = getCmd (messageContent msg)
  case cmd of
    Right CreatePoll -> createPoll msg
    Right TellJoke -> tellJoke jokes msg
    Left _ -> unknown msg

createPoll :: Message -> DiscordHandler ()
createPoll msg = do
  P.PollResponse url <- liftIO P.createPoll -- TODO retry if didn't succeed
  void $ restCall (R.CreateMessage (messageChannelId msg) url) -- TODO retry this too

tellJoke :: [Text] -> Message -> DiscordHandler ()
tellJoke jokes msg = do
  joke <- (jokes !!) <$> randomRIO (0, length jokes - 1)
  void $ restCall (R.CreateMessage (messageChannelId msg) joke) -- TODO retry this too

unknown :: Message -> DiscordHandler ()
unknown msg = void $ restCall (R.CreateMessage (messageChannelId msg) "Sorry, didn't understand :(")

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
