{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot where

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
  MessageCreate msg -> handleMessage reply env msg
  -- MessageCreate m -> when (isPing m && not (fromBot m)) $ do
  --   void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
  --   threadDelay (2 * 10 ^ 6)
  --   void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
  _ -> return ()

handleMessage ::
  (Message -> Text -> DiscordHandler ()) ->
  Env ->
  Message ->
  DiscordHandler ()
handleMessage replyCmd Env{..} msg = unless (fromBot msg) $ do
  let cmd = getCmd (messageContent msg)
      withAuth = auth adminId msg
  liftIO $ withLog "Got command: " (pure cmd)
  case cmd of
    -- Admin only
    Right CreatePoll -> withAuth $ createPoll replyCmd configRef strawPollToken chanId
    Right ShutUp -> withAuth $ stopPolls replyCmd configRef msg
    Right (ScheduleHours slots) -> withAuth $ scheduleHours replyCmd configRef slots msg
    Right (ScheduleDays days) -> withAuth $ scheduleDays replyCmd configRef days msg
    Right GetSchedule -> withAuth $ getSchedule replyCmd configRef msg
    Right GetSlots -> withAuth $ getSlots replyCmd configRef msg
    -- All users
    Right (Call target) -> call replyCmd target msg
    Right TellJoke -> tellJoke replyCmd jokes msg
    Right UnknownCommand -> unknown replyCmd msg
    Left _ -> pure ()

createPoll ::
  (Message -> Text -> DiscordHandler ()) ->
  IORef P.Config ->
  Text ->
  ChannelId ->
  DiscordHandler ()
createPoll replyCmd configRef token chanId = do
  config <- liftIO $ readIORef configRef
  unless (null config.days) $ do
    P.PollResponse url <- liftIO $ P.createPoll config token -- TODO retry if didn't succeed
    void $ restCall (R.CreateMessage chanId url) -- TODO retry this too

stopPolls ::
  (Message -> Text -> DiscordHandler ()) ->
  IORef P.Config ->
  Message ->
  DiscordHandler ()
stopPolls replyCmd configRef msg = do
  liftIO $ modifyConfig configRef (\currConf -> currConf{P.days = []})
  replyCmd msg "All right, turning off polls."

scheduleHours ::
  (Message -> Text -> DiscordHandler ()) ->
  IORef P.Config ->
  [Int] ->
  Message ->
  DiscordHandler ()
scheduleHours replyCmd configRef slots@[from, to] msg = do
  liftIO $ modifyConfig configRef (\currConf -> currConf{P.slots = slots})
  replyCmd msg (T.pack (printf "Timeslots were modified to: %d - %d" from to))

scheduleDays ::
  (Message -> Text -> DiscordHandler ()) ->
  IORef P.Config ->
  [Int] ->
  Message ->
  DiscordHandler ()
scheduleDays replyCmd configRef days msg = do
  liftIO $ modifyConfig configRef (\currConf -> currConf{P.days = days})
  replyCmd msg (T.pack ("Schedule was modified to:" <> concatMap ((' ' :) . P.showDay) days))

getSchedule ::
  (Message -> Text -> DiscordHandler ()) ->
  IORef P.Config ->
  Message ->
  DiscordHandler ()
getSchedule replyCmd configRef msg = do
  (P.Config days _) <- liftIO $ readIORef configRef
  replyCmd msg (T.pack ("Schedule for poll is:" <> concatMap ((' ' :) . P.showDay) days))

getSlots ::
  (Message -> Text -> DiscordHandler ()) ->
  IORef P.Config ->
  Message ->
  DiscordHandler ()
getSlots replyCmd configRef msg = do
  (P.Config _ [from, to]) <- liftIO $ readIORef configRef
  replyCmd msg (T.pack (printf "Timeslots for poll: %d - %d" from to))

modifyConfig :: IORef P.Config -> (P.Config -> P.Config) -> IO ()
modifyConfig configRef f = do
  modifyIORef configRef f
  newConfig <- readIORef configRef
  BS.writeFile configPath (encode newConfig)

call ::
  (Message -> Text -> DiscordHandler ()) ->
  String ->
  Message ->
  DiscordHandler ()
call replyCmd target msg = do
  dow <- liftIO (Data.Time.dayOfWeek . utctDay <$> getCurrentTime)
  if (target == "Tüskecsarnok" || target == "Tüske") && dow < Saturday
    then replyCmd msg "They said they are full and then condescendingly reprimanded me for calling them in the first place during the working days of the week."
    else replyCmd msg "Unfortunately, no response..."

tellJoke ::
  (Message -> Text -> DiscordHandler ()) ->
  [Text] ->
  Message ->
  DiscordHandler ()
tellJoke replyCmd jokes msg = do
  joke <- (jokes !!) <$> randomRIO (0, length jokes - 1)
  replyCmd msg joke -- TODO retry this too

unknown ::
  (Message -> Text -> DiscordHandler ()) ->
  Message ->
  DiscordHandler ()
unknown replyCmd msg = replyCmd msg "Sorry, didn't understand :("

reply :: Message -> Text -> DiscordHandler ()
reply msg = void . restCall . R.CreateMessage (messageChannelId msg)

auth :: UserId -> Message -> DiscordHandler () -> DiscordHandler ()
auth admin msg = when (fromAdmin admin msg)

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

fromAdmin :: UserId -> Message -> Bool
fromAdmin admin = (== admin) . userId . messageAuthor
