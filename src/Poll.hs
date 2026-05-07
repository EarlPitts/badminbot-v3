{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poll where

import Data.Time.Calendar
import Data.Time.Clock as C

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime

defaultDays = weekAllDays Monday . firstDayOfWeekOnAfter Monday
defaultHours = [start, start + oneHour .. (end - oneHour)]
 where
  start = 8 * oneHour
  end = 22 * oneHour

oneHour :: DiffTime
oneHour = 3600

data Poll = Poll
  { title :: String
  , options :: [Event]
  }
  deriving (Show)

instance ToJSON Poll where
  toJSON Poll{..} =
    object
      [ "title" .= title
      , "type" .= ("meeting" :: Text)
      , "poll_options" .= options
      , "poll_config"
          .= object
            [ "vote_type" .= ("participant_grid" :: Text)
            , "is_private" .= True
            , "is_multiple_choice" .= True
            , "edit_vote_permissions" .= ("admin_voter" :: Text)
            , "allow_indeterminate" .= True
            , "duplication_checking" .= ("ip" :: Text)
            ]
      ]

data Event = Event
  { startTime :: C.UTCTime
  , endTime :: C.UTCTime
  }
  deriving (Show)

instance ToJSON Event where
  toJSON Event{..} =
    object
      [ "start_time" .= utcTimeToPOSIXSeconds startTime
      , "end_time" .= utcTimeToPOSIXSeconds endTime
      , "type" .= ("time_range" :: Text)
      ]

mkPoll :: TimeZone -> String -> [Day] -> [DiffTime] -> Poll
mkPoll tz title days hours = Poll{..}
 where
  options = mkEvent tz <$> days <*> hours

mkEvent :: TimeZone -> Day -> DiffTime -> Event
mkEvent tz day start = Event (shiftTz start) (shiftTz (start + oneHour))
 where
  shiftTz = localTimeToUTC tz . LocalTime day . timeToTimeOfDay

f = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  let d = utctDay t
  let title = "sajt"
  pure $ mkEvent tz d <$> defaultHours
