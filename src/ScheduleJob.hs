{-# LANGUAGE NumericUnderscores #-}

module ScheduleJob where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Reader
import Data.Time
import Discord

data DayTime = DayTime
  { dayOfWeek :: DayOfWeek
  , timeOfDay :: TimeOfDay
  }
  deriving (Show)

data Job = Job
  { time :: DayTime
  , action :: DiscordHandler ()
  }

schedule :: Job -> DiscordHandler (Async ())
schedule Job{..} = do
  handle <- ask
  liftIO $ async $ forever $ do
    now <- getCurrentTime
    tz <- getCurrentTimeZone
    let delay = getDelay tz now time
    when (delay > 0) $ do
      -- TODO add log for scheduled job
      threadDelay (floor (delay * 1_000_000))
      runReaderT action handle

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
