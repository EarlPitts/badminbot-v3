{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot (runBot) where

import Command
import Control.Applicative
import Control.Monad (guard, unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import System.Environment
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)
import Text.Printf (printf)
import Prelude hiding (lines, log)

import Discord
import qualified Discord.Requests as R
import Discord.Types

import qualified Poll as P
import ScheduleJob

data Env = Env
  { configRef :: IORef P.Config
  , jokes :: [Text]
  , timeZone :: TimeZone
  , token :: Text
  , chanId :: ChannelId
  , adminId :: UserId
  , pollPublishTime :: DayTime
  , strawPollToken :: Text
  }

type App = ReaderT Env IO

configPath, jokesPath :: String
jokesPath = "jokes.txt"
configPath = "config.json"

runBot :: IO ()
runBot = do
  hSetBuffering stdout LineBuffering
  jokes <- T.lines <$> TIO.readFile jokesPath
  timeZone <- getCurrentTimeZone
  token <- T.pack <$> getEnv "TOKEN"
  chanId <- DiscordId . Snowflake . read <$> getEnv "CHAN_ID"
  adminId <- DiscordId . Snowflake . read <$> getEnv "ADMIN"
  strawPollToken <- T.pack <$> getEnv "STRAWPOLL_TOKEN"
  config <-
    withLog "Read config" (readConfig configPath)
      <|> withLog "Using default config" (pure P.defaultConfig)
  configRef <- newIORef config
  let pollPublishTime = DayTime Thursday (TimeOfDay 11 0 0)
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

readConfig :: String -> IO P.Config
readConfig path = BS.readFile path >>= throwDecode

schedulePolls :: Env -> DiscordHandler ()
schedulePolls env = do
  handle <- ask
  liftIO $ void $ schedule env.timeZone getCurrentTime $ Job env.pollPublishTime $ do
    config <- readIORef env.configRef
    guard (not . null $ config.days)
    P.PollResponse url <- P.createPoll config env.strawPollToken
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
  liftIO $ withLog "Got command: " (pure cmd)
  case cmd of
    -- Admin only
    Right CreatePoll -> withAuth $ createPoll configRef strawPollToken chanId
    Right ShutUp -> withAuth $ stopPolls configRef msg
    Right (ScheduleHours slots) -> withAuth $ scheduleHours configRef slots msg
    Right (ScheduleDays days) -> withAuth $ scheduleDays configRef days msg
    Right GetSchedule -> withAuth $ getSchedule configRef msg
    Right GetSlots -> withAuth $ getSlots configRef msg
    -- All users
    Right TellJoke -> tellJoke jokes msg
    Right UnknownCommand -> unknown msg
    Left _ -> pure ()

createPoll :: IORef P.Config -> Text -> ChannelId -> DiscordHandler ()
createPoll configRef token chanId = do
  config <- liftIO $ readIORef configRef
  unless (null config.days) $ do
    P.PollResponse url <- liftIO $ P.createPoll config token -- TODO retry if didn't succeed
    void $ restCall (R.CreateMessage chanId url) -- TODO retry this too

stopPolls :: IORef P.Config -> Message -> DiscordHandler ()
stopPolls configRef msg = do
  liftIO $ modifyConfig configRef (\currConf -> currConf{P.days = []})
  reply msg "All right, turning off polls."

scheduleHours :: IORef P.Config -> [Int] -> Message -> DiscordHandler ()
scheduleHours configRef slots@[from, to] msg = do
  liftIO $ modifyConfig configRef (\currConf -> currConf{P.slots = slots})
  reply msg (T.pack (printf "Timeslots were modified to: %d - %d" from to))

scheduleDays :: IORef P.Config -> [Int] -> Message -> DiscordHandler ()
scheduleDays configRef days msg = do
  liftIO $ modifyConfig configRef (\currConf -> currConf{P.days = days})
  reply msg (T.pack ("Schedule was modified to:" <> concatMap ((' ' :) . P.showDay) days))

getSchedule :: IORef P.Config -> Message -> DiscordHandler ()
getSchedule configRef msg = do
  (P.Config days _) <- liftIO $ readIORef configRef
  reply msg (T.pack ("Schedule for poll is:" <> concatMap ((' ' :) . P.showDay) days))

getSlots :: IORef P.Config -> Message -> DiscordHandler ()
getSlots configRef msg = do
  (P.Config _ [from, to]) <- liftIO $ readIORef configRef
  reply msg (T.pack (printf "Timeslots for poll: %d - %d" from to))

modifyConfig :: IORef P.Config -> (P.Config -> P.Config) -> IO ()
modifyConfig configRef f = do
  modifyIORef configRef f
  newConfig <- readIORef configRef
  BS.writeFile configPath (encode newConfig)

tellJoke :: [Text] -> Message -> DiscordHandler ()
tellJoke jokes msg = do
  joke <- (jokes !!) <$> randomRIO (0, length jokes - 1)
  reply msg joke -- TODO retry this too

unknown :: Message -> DiscordHandler ()
unknown msg = reply msg "Sorry, didn't understand :("

reply :: Message -> Text -> DiscordHandler ()
reply msg = void . restCall . R.CreateMessage (messageChannelId msg)

auth :: UserId -> Message -> DiscordHandler () -> DiscordHandler ()
auth admin msg = when (fromAdmin admin msg)

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

fromAdmin :: UserId -> Message -> Bool
fromAdmin admin = (== admin) . userId . messageAuthor
