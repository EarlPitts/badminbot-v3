{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bot
import Command
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Text (Text)
import Data.Time
import GHC.Conc (threadDelay)
import qualified Poll as P
import ScheduleJob
import Test.Hspec

main :: IO ()
main = do
  hspec do
    -- Integration
    describe "HandleCommand" do
      it "should reply with a joke" do
        testJoke
      it "should reply with the poll url" do
        testCreatePoll

testJoke :: IO ()
testJoke = do
  let cmd = TellJoke
      env = Env{jokes = ["funny joke"]}

  replyMsg <- handleCommand env id cmd

  replyMsg `shouldBe` "funny joke"

testCreatePoll :: IO ()
testCreatePoll = do
  configRef <- newIORef P.defaultConfig
  let cmd = CreatePoll
      env = Env{createPollFun = pollServiceStub, configRef = configRef}

  replyMsg <- handleCommand env id cmd

  replyMsg `shouldBe` "poll url"

pollServiceStub :: a -> b -> IO P.PollResponse
pollServiceStub _ _ = pure (P.PollResponse "poll url")

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
