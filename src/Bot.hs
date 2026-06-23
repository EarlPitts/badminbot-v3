module Bot where

import Command
import Control.Applicative
import Control.Monad (guard, unless, void)
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
import System.Timeout (timeout)
import Text.Printf (printf)
import Prelude hiding (lines, log)

import BrainFuck (evalWith)
import Discord
import qualified Discord.Requests as R
import Discord.Types

import Control.Exception
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
  , createPollFun :: P.Config -> Text -> IO P.PollResponse
  }

configPath, jokesPath :: String
jokesPath = "jokes.txt"
configPath = "config.json"

main :: IO ()
main = do
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
      createPollFun = P.createPoll
  runBot Env{..}

runBot :: Env -> IO ()
runBot env = do
  putStrLn "started..."
  userFacingError <-
    runDiscord $
      def
        { discordToken = "Bot " <> env.token
        , discordOnStart = schedulePolls env
        , discordOnEvent = eventHandler env
        , discordOnLog = TIO.putStrLn
        }
  TIO.putStrLn userFacingError
  exitFailure

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
  _ -> return ()

handleMessage :: Env -> Message -> DiscordHandler ()
handleMessage env@Env{..} msg = unless (fromBot msg) $ do
  let cmd = getCmd (messageContent msg)
      withAuth = auth adminId msg
  liftIO $ withLog "Got command: " (pure cmd)
  case cmd of
    Right cmd' -> liftIO (handleCommand env withAuth cmd') >>= reply (messageChannelId msg)
    Left _ -> pure ()

handleCommand :: Env -> (IO Text -> IO Text) -> Command -> IO Text
handleCommand Env{..} withAuth = \case
  -- Admin only
  CreatePoll -> withAuth $ createPoll createPollFun configRef strawPollToken
  ShutUp -> withAuth $ stopPolls configRef
  (ScheduleHours slots) -> withAuth $ scheduleHours configRef slots
  (ScheduleDays days) -> withAuth $ scheduleDays configRef days
  GetSchedule -> withAuth $ getSchedule configRef
  GetSlots -> withAuth $ getSlots configRef
  -- All users
  (Call target) -> call target
  TellJoke -> tellJoke jokes
  Eval progStr -> evalProgram progStr
  UnknownCommand -> pure "Sorry, didn't understand :("

createPoll :: (P.Config -> Text -> IO P.PollResponse) -> IORef P.Config -> Text -> IO Text
createPoll createPollFun configRef token = do
  config <- readIORef configRef
  if null config.days
    then pure ""
    else do
      P.PollResponse url <- createPollFun config token -- TODO retry if didn't succeed
      pure url

stopPolls :: IORef P.Config -> IO Text
stopPolls configRef = do
  modifyConfig configRef (\currConf -> currConf{P.days = []})
  pure "All right, turning off polls."

scheduleHours :: IORef P.Config -> [Int] -> IO Text
scheduleHours configRef slots@[from, to] = do
  modifyConfig configRef (\currConf -> currConf{P.slots = slots})
  pure (T.pack (printf "Timeslots were modified to: %d - %d" from to))

scheduleDays :: IORef P.Config -> [Int] -> IO Text
scheduleDays configRef days = do
  if any (\n -> n < 0 || n > 6) days || null days
    then pure "Days should be numbers between 0 and 6, please!"
    else do
      modifyConfig configRef (\currConf -> currConf{P.days = days})
      pure (T.pack ("Schedule was modified to:" <> concatMap ((' ' :) . P.showDay) days))

getSchedule :: IORef P.Config -> IO Text
getSchedule configRef = do
  (P.Config days _) <- readIORef configRef
  pure (T.pack ("Schedule for poll is:" <> concatMap ((' ' :) . P.showDay) days))

getSlots :: IORef P.Config -> IO Text
getSlots configRef = do
  (P.Config _ [from, to]) <- readIORef configRef
  pure (T.pack (printf "Timeslots for poll: %d - %d" from to))

modifyConfig :: IORef P.Config -> (P.Config -> P.Config) -> IO ()
modifyConfig configRef f = do
  modifyIORef configRef f
  newConfig <- readIORef configRef
  BS.writeFile configPath (encode newConfig)

call :: String -> IO Text
call target = do
  dow <- Data.Time.dayOfWeek . utctDay <$> getCurrentTime
  pure $
    if (target == "Tüskecsarnok" || target == "Tüske") && dow < Saturday
      then "They said they are full and then condescendingly reprimanded me for calling them in the first place during the working days of the week."
      else "Unfortunately, no response..."

tellJoke :: [Text] -> IO Text
tellJoke jokes = (jokes !!) <$> randomRIO (0, length jokes - 1)

evalProgram :: String -> IO Text
evalProgram progStr =
  timeout 2000000 (evaluate (T.pack $ evalWith progStr "")) >>= \case
    Nothing -> pure "Give me something that terminates!"
    Just result -> pure result

reply :: ChannelId -> Text -> DiscordHandler ()
reply channelId = void . restCall . R.CreateMessage channelId

auth :: UserId -> Message -> IO Text -> IO Text
auth admin msg action =
  if fromAdmin admin msg
    then action
    else pure "Sorry, cannot do that. :("

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

fromAdmin :: UserId -> Message -> Bool
fromAdmin admin = (== admin) . userId . messageAuthor
