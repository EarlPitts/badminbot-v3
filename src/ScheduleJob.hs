{-# LANGUAGE NumericUnderscores #-}

module ScheduleJob where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Reader
import Data.Time

data DayTime = DayTime
  { dayOfWeek :: DayOfWeek
  , timeOfDay :: TimeOfDay
  }
  deriving (Show)

data Job = Job
  { time :: DayTime
  , action :: IO ()
  }

schedule :: TimeZone -> IO UTCTime -> Job -> IO (Async ())
schedule tz getTime Job{..} = do
  liftIO $ async $ forever $ do
    now <- getTime
    let delay = getDelay tz now time
    when (delay > 0) $ do
      putStrLn $ "scheduled poll after " <> show delay <> " seconds"
      -- TODO add log for scheduled job
      threadDelay (floor (delay * 1_000_000))
      action

getDelay :: TimeZone -> UTCTime -> DayTime -> NominalDiffTime
getDelay tz now DayTime{..} = diffUTCTime nextJob now
 where
  day = utctDay now
  jobTime = localTimeToUTC tz (LocalTime day timeOfDay)
  nextDay =
    if jobTime < now
      then firstDayOfWeekOnAfter dayOfWeek (addDays 1 day)
      else firstDayOfWeekOnAfter dayOfWeek day
  nextJob = localTimeToUTC tz (LocalTime nextDay timeOfDay)
