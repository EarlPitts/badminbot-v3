{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Bot
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Text (Text)
import Data.Time
import Discord
import Discord.Handle
import qualified Discord.Requests as R
import Discord.Types
import GHC.Conc (threadDelay)
import ScheduleJob
import Test.Hspec

main :: IO ()
main = do
  hspec do
    -- docker <- runIO Docker.mkService
    -- let runner = Runner.mkService docker

    -- beforeAll (print "sajt") $

    -- describe "Scheduler" do
    --   it "should get the delay right" do
    --     testDelay

    -- Integration
    describe "HandleMessage" do
      -- it "should create the poll" do
      --   testCreatePoll

      it "should reply with a joke" do
        testJoke

admin = DiscordId (Snowflake 123)
user = DiscordId (Snowflake 234)

mkFakeReply :: IO (IORef Text, Message -> Text -> DiscordHandler ())
mkFakeReply = do
  replyRef <- newIORef ""
  pure
    ( replyRef
    , \_ replyText -> do
        liftIO $ writeIORef replyRef replyText
    )

mkMessage :: UserId -> Text -> Message
mkMessage userId content =
  let user =
        User
          { userId = userId
          , userIsBot = False
          }
   in Message
        { messageId = DiscordId (Snowflake 123)
        , messageChannelId = DiscordId (Snowflake 123)
        , messageContent = content
        , messageAuthor = user
        }

testCreatePoll :: IO ()
testCreatePoll = do
  (replyTextRef, fakeReply) <- mkFakeReply
  let msg = mkMessage admin "!poll"
      handle = DiscordHandle{}
      env = Env{adminId = admin }

  runReaderT (handleMessage fakeReply env msg) handle

  replyText <- readIORef replyTextRef
  replyText `shouldBe` ""

testJoke :: IO ()
testJoke = do
  (replyTextRef, fakeReply) <- mkFakeReply
  let msg = mkMessage user "!joke"
      handle = DiscordHandle{}
      env = Env{jokes = ["funny joke"]}

  runReaderT (handleMessage fakeReply env msg) handle

  replyText <- readIORef replyTextRef
  replyText `shouldBe` "unny joke"

mkFakeTime :: UTCTime -> IO (IORef UTCTime)
mkFakeTime = newIORef

progress :: NominalDiffTime -> IORef UTCTime -> IO ()
progress delta = flip modifyIORef (addUTCTime delta)

getTime :: IORef UTCTime -> IO UTCTime
getTime = readIORef

testDelay :: IO ()
testDelay = do
  tz <- getCurrentTimeZone
  timeRef <- mkFakeTime (read "2026-05-19 12:00:00 UTC")
  done <- newEmptyMVar
  let jobTime = DayTime Tuesday (TimeOfDay 15 0 0)
  let action = putMVar done ()

  schedule tz (getTime timeRef) (Job jobTime action)
  threadDelay 1000000
  progress 3600 timeRef
  print "sajt"

  readMVar done
