{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Poll where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock as C
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime
import GHC.Generics (Generic)
import Network.HTTP.Req

data Config = Config
  { days :: [Int]
  , slots :: [Int]
  }
  deriving (Show, FromJSON, Generic)

newtype PollResponse = PollResponse
  { url :: Text
  }
  deriving (Show)

instance FromJSON PollResponse where
  parseJSON = withObject "PollResponse" $ \obj ->
    PollResponse <$> obj .: "url"

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

defaultDays = nextWeek
defaultSlots = mkSlots 8 22

mkSlots :: DiffTime -> DiffTime -> [DiffTime]
mkSlots startNum endNum = [start, start + oneHour .. (end - oneHour)]
 where
  start = startNum * oneHour
  end = endNum * oneHour

nextWeek :: Day -> [Day]
nextWeek = weekAllDays Monday . firstDayOfWeekOnAfter Monday

weekNum :: Day -> Int
weekNum d =
  let (_, week, _) = toWeekDate d
   in week

oneHour :: DiffTime
oneHour = 3600

getDays :: Maybe Config -> Day -> [Day]
getDays Nothing d = defaultDays d
getDays (Just (Config dayIdxs _)) d =
  snd <$> filter (\(i, _) -> i `elem` dayIdxs) (zip [0 ..] (nextWeek d))

getSlots :: Maybe Config -> [DiffTime]
getSlots Nothing = defaultSlots
getSlots (Just (Config _ boundaries)) = mkSlots start finish
 where
  [start, finish] = secondsToDiffTime . fromIntegral <$> boundaries

mkPoll :: TimeZone -> String -> [Day] -> [DiffTime] -> Poll
mkPoll tz title days hours = Poll{..}
 where
  options = mkEvent tz <$> days <*> hours

mkEvent :: TimeZone -> Day -> DiffTime -> Event
mkEvent tz day start = Event (shiftTz start) (shiftTz (start + oneHour))
 where
  shiftTz = localTimeToUTC tz . LocalTime day . timeToTimeOfDay

createPoll :: Maybe Config -> Text -> IO PollResponse
createPoll maybeConfig token = do
  d <- utctDay <$> getCurrentTime
  tz <- getCurrentTimeZone

  let
    week = getDays maybeConfig d
    slots = getSlots maybeConfig
    wNum = show $ weekNum (head week)
    title = "Tollas (hét #" <> wNum <> ")"
    poll = mkPoll tz title week slots
    request =
      req
        POST
        (https "api.strawpoll.com" /: "v3/polls")
        (ReqBodyJson poll)
        jsonResponse
        (header "X-API-Key" (encodeUtf8 token))

  responseBody <$> runReq defaultHttpConfig request
