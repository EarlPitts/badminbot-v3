{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot (runBot) where

import Control.Monad
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Data.Text (Text, isPrefixOf, lines, pack, toLower)
import qualified Data.Text.IO as TIO
import Data.Time
import System.Environment
import System.Random (randomRIO)
import UnliftIO.Concurrent
import Prelude hiding (lines)

import Discord
import qualified Discord.Requests as R
import Discord.Types

import Command
import Control.Concurrent.Async
import Control.Monad.Reader
import Data.Foldable (for_, traverse_)
import Data.Word
import qualified Poll as P
import ScheduleJob
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

data Env = Env
  { -- config :: Config
    jokes :: [Text]
  , timeZone :: TimeZone
  , token :: Text
  , chanId :: ChannelId
  , pollTimes :: [DayTime]
  , strawPollToken :: Text
  }
  deriving (Show)

-- data Config = Config
--   { days :: [Int]
--   , slots :: [Int]
--   }
--   deriving (Show)

type App = ReaderT Env IO

runBot :: IO ()
runBot = do
  hSetBuffering stdout LineBuffering
  jokes <- lines <$> TIO.readFile "jokes.txt"
  timeZone <- getCurrentTimeZone
  token <- pack <$> getEnv "TOKEN"
  chanId <- DiscordId . Snowflake . read <$> getEnv "CHAN_ID"
  strawPollToken <- pack <$> getEnv "STRAWPOLL_TOKEN"
  let pollTimes = [DayTime Thursday (TimeOfDay 11 0 0)]
  runReaderT badminbot Env{..}

badminbot :: App ()
badminbot = do
  liftIO $ putStrLn "started..."
  env <- ask
  userFacingError <-
    liftIO $
      runDiscord $
        def
          { discordToken = "Bot " <> env.token
          , discordOnStart = schedulePolls env
          , discordOnEvent = eventHandler env
          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
          }
  liftIO $ TIO.putStrLn userFacingError

-- userFacingError is an unrecoverable error
-- put normal 'cleanup' code in discordOnEnd (see examples)

schedulePolls :: Env -> DiscordHandler ()
schedulePolls env = do
  handle <- ask
  liftIO $ for_ env.pollTimes $ \time ->
    schedule env.timeZone getCurrentTime $ Job time $ do
      P.PollResponse url <- P.createPoll env.strawPollToken
      void $ runReaderT (restCall (R.CreateMessage env.chanId url)) handle

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
    Right CreatePoll -> createPoll strawPollToken msg
    Right TellJoke -> tellJoke jokes msg
    Left _ -> unknown msg

createPoll :: Text -> Message -> DiscordHandler ()
createPoll token msg = do
  P.PollResponse url <- liftIO $ P.createPoll token -- TODO retry if didn't succeed
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
