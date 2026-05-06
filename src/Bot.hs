{-# LANGUAGE OverloadedStrings #-}

module Bot (runBot) where

import Control.Monad (unless, void, when)
import Data.Text (Text, isPrefixOf, pack, toLower)
import qualified Data.Text.IO as TIO
import System.Environment
import UnliftIO.Concurrent

import Control.Monad.IO.Class
import Discord
import qualified Discord.Requests as R
import Discord.Types
import qualified Poll as P

data Command = CreatePoll

runBot :: IO ()
runBot = do
  token <- pack <$> getEnv "TOKEN"
  badminbot token

badminbot :: Text -> IO ()
badminbot token = do
  putStrLn "started..."
  userFacingError <-
    runDiscord $
      def
        { discordToken = "Bot " <> token
        , discordOnEvent = eventHandler
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }
  TIO.putStrLn userFacingError

-- userFacingError is an unrecoverable error
-- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate msg -> handleMessage msg
  -- MessageCreate m -> when (isPing m && not (fromBot m)) $ do
  --   void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
  --   threadDelay (2 * 10 ^ 6)
  --   void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
  _ -> return ()

handleMessage :: Message -> DiscordHandler ()
handleMessage msg = unless (fromBot msg) $ do
  let cmd = getCmd (messageContent msg)
  case cmd of
    Just CreatePoll -> undefined
    Nothing -> void $ restCall (R.CreateMessage (messageChannelId msg) "Sorry, didn't understand :(")


getCmd :: Text -> Maybe Command
getCmd msg = Nothing

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
