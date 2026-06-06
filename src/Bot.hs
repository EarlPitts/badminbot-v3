{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot (runBot) where

import Command
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (for_)
import Data.Text (Text, isPrefixOf, lines, pack, toLower)
import qualified Data.Text.IO as TIO
import Data.Time
import System.Directory (doesFileExist)
import System.Environment
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)
import Prelude hiding (lines, log)

import Discord
import qualified Discord.Requests as R
import Discord.Types

import qualified Poll as P
import ScheduleJob

data Env = Env
  { config :: Maybe P.Config
  , jokes :: [Text]
  , timeZone :: TimeZone
  , token :: Text
  , chanId :: ChannelId
  , adminId :: UserId
  , pollPublishTime :: [DayTime]
  , strawPollToken :: Text
  }
  deriving (Show)

type App = ReaderT Env IO

configPath, jokesPath :: String
jokesPath = "jokes.txt"
configPath = "config.json"

runBot :: IO ()
runBot = do
  hSetBuffering stdout LineBuffering
  jokes <- lines <$> TIO.readFile jokesPath
  timeZone <- getCurrentTimeZone
  token <- pack <$> getEnv "TOKEN"
  chanId <- DiscordId . Snowflake . read <$> getEnv "CHAN_ID"
  adminId <- DiscordId . Snowflake . read <$> getEnv "ADMIN"
  strawPollToken <- pack <$> getEnv "STRAWPOLL_TOKEN"
  config <- withLog "Read config" (readConfig configPath)
  let pollPublishTime = [DayTime Thursday (TimeOfDay 11 0 0)]
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
  liftIO exitFailure

-- userFacingError is an unrecoverable error
-- put normal 'cleanup' code in discordOnEnd (see examples)

withLog :: (Show a) => String -> IO a -> IO a
withLog msg f = do
  res <- f
  putStrLn $ msg <> ": " <> show res
  pure res

readConfig :: String -> IO (Maybe P.Config)
readConfig path = do
  exists <- doesFileExist path
  if exists
    then throwDecode =<< BS.readFile path
    else pure Nothing

schedulePolls :: Env -> DiscordHandler ()
schedulePolls env = do
  handle <- ask
  liftIO $ for_ env.pollPublishTime $ \time ->
    schedule env.timeZone getCurrentTime $ Job time $ do
      P.PollResponse url <- P.createPoll env.config env.strawPollToken
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
      withAuth = auth adminId msg
  case cmd of
    -- Admin only
    Right CreatePoll -> withAuth $ createPoll config strawPollToken msg
    -- All users
    Right TellJoke -> tellJoke jokes msg
    Right UnknownCommand -> unknown msg
    Left _ -> pure ()

createPoll :: Maybe P.Config -> Text -> Message -> DiscordHandler ()
createPoll config token msg = do
  P.PollResponse url <- liftIO $ P.createPoll config token -- TODO retry if didn't succeed
  void $ restCall (R.CreateMessage (messageChannelId msg) url) -- TODO retry this too

tellJoke :: [Text] -> Message -> DiscordHandler ()
tellJoke jokes msg = do
  joke <- (jokes !!) <$> randomRIO (0, length jokes - 1)
  void $ restCall (R.CreateMessage (messageChannelId msg) joke) -- TODO retry this too

unknown :: Message -> DiscordHandler ()
unknown msg = void $ restCall (R.CreateMessage (messageChannelId msg) "Sorry, didn't understand :(")

auth :: UserId -> Message -> DiscordHandler () -> DiscordHandler ()
auth admin msg = when (fromAdmin admin msg)

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

fromAdmin :: UserId -> Message -> Bool
fromAdmin admin = (== admin) . userId . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
