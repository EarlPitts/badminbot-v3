{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Poll where

import Data.Time.Calendar
import Data.Time.Calendar
import Data.Time.Clock as C

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics
import Network.HTTP.Req
import qualified Text.URI as URI

defaultDays = weekAllDays Monday . firstDayOfWeekOnAfter Monday
defaultHours = secondsToDiffTime <$> [8 .. 22]

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

mkPoll :: String -> [Day] -> [DiffTime] -> Poll
mkPoll title days hours = Poll{..}
 where
  options = mkEvents days hours

mkEvents :: [Day] -> [DiffTime] -> [Event]
mkEvents days hours = mkEvent <$> days <*> hours

mkEvent :: Day -> DiffTime -> Event
mkEvent day hour =
  Event
    { startTime = UTCTime day (hour * 3600)
    , endTime = UTCTime day ((hour + 1) * 3600)
    }

f = do
  t <- getCurrentTime
  let d = utctDay t
  let title = "sajt"

  pure $ mkPoll title [d] defaultHours

main :: IO ()
main = do
  d <- utctDay <$> getCurrentTime
  runReq defaultHttpConfig $ do
    let myData = mkPoll "testpoll" (defaultDays d) defaultHours
    v <- req POST (https "api.strawpoll.com" /: "v3/polls") (ReqBodyJson myData) jsonResponse mempty
    liftIO $ print (responseBody v :: Value)
