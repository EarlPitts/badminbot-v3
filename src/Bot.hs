{-# LANGUAGE OverloadedStrings #-}

module Bot (runBot) where

import Control.Monad (void, when)
import Data.Text (Text, isPrefixOf, pack, toLower)
import qualified Data.Text.IO as TIO
import System.Environment
import UnliftIO.Concurrent

import Discord
import qualified Discord.Requests as R
import Discord.Types

runBot :: IO ()
runBot = do
  token <- pack <$> getEnv "TOKEN"
  pingpongExample token

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: Text -> IO ()
pingpongExample token = do
  userFacingError <-
    runDiscord $
      def
        { discordToken = "Bot " <> token
        , discordOnEvent = eventHandler
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        } -- if you see OnLog error, post in the discord / open an issue
  TIO.putStrLn userFacingError

-- userFacingError is an unrecoverable error
-- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (isPing m && not (fromBot m)) $ do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    threadDelay (2 * 10 ^ 6)
    void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
  _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
