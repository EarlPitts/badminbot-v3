{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bot hiding (main)
import Command
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Data.IORef
import qualified Data.Text as T
import Data.Time
import GHC.Conc (threadDelay)
import qualified Poll as P
import ScheduleJob
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = do
  hspec do
    -- Integration
    describe "HandleCommand" do
      it "should reply with a joke" do
        testJoke
      it "should reply with the poll url" do
        testCreatePoll
      it "should turn off polls" do
        testShutUp
      it "should schedule the specified timeslots" do
        testScheduleHours
      prop "should schedule the specified days" do
        forAll (listOf1 (choose (0, 6))) testScheduleDays
      prop "should not schedule the specified days when data is invalid" do
        testScheduleDaysInvalid . filter (\n -> n < 0 || n > 6)
      prop "reply with the scheduled days" do
        forAll (listOf1 (choose (0, 6))) testGetSchedule

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

testShutUp :: IO ()
testShutUp = do
  configRef <- newIORef P.defaultConfig
  let cmd = ShutUp
      env = Env{configRef = configRef}

  replyMsg <- handleCommand env id cmd

  modifiedConfig <- readIORef configRef
  modifiedConfig `shouldBe` P.Config [] P.defaultConfig.slots
  replyMsg `shouldBe` "All right, turning off polls."

testScheduleHours :: IO ()
testScheduleHours = do
  configRef <- newIORef P.defaultConfig
  let cmd = ScheduleHours [4, 16]
      env = Env{configRef = configRef}

  replyMsg <- handleCommand env id cmd

  modifiedConfig <- readIORef configRef
  modifiedConfig `shouldBe` P.Config P.defaultConfig.days [4, 16]
  replyMsg `shouldBe` "Timeslots were modified to: 4 - 16"

testScheduleDays :: [Int] -> IO ()
testScheduleDays days = do
  configRef <- newIORef P.defaultConfig
  let cmd = ScheduleDays days
      env = Env{configRef = configRef}

  _ <- handleCommand env id cmd

  modifiedConfig <- readIORef configRef
  modifiedConfig `shouldBe` P.Config days P.defaultConfig.slots

testScheduleDaysInvalid :: [Int] -> IO ()
testScheduleDaysInvalid days = do
  configRef <- newIORef P.defaultConfig
  let cmd = ScheduleDays days
      env = Env{configRef = configRef}

  replyMsg <- handleCommand env id cmd

  modifiedConfig <- readIORef configRef
  replyMsg `shouldBe` "Days should be numbers between 0 and 6, please!"
  modifiedConfig `shouldBe` P.defaultConfig

testGetSchedule :: [Int] -> IO ()
testGetSchedule days = do
  configRef <- newIORef $ P.Config days []
  let cmd = GetSchedule
      env = Env{configRef = configRef}

  replyMsg <- handleCommand env id cmd

  T.unpack replyMsg `shouldContain` "Schedule for poll is:"

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
