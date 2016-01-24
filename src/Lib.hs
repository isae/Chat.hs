{-# LANGUAGE OverloadedStrings          #-}

module Lib
    (
      serverPort,
      serverUrl,
      ChatMessage(..)
    ) where

import Data.Time.Clock
import Data.Aeson

serverPort :: Int
serverPort = 8000

serverUrl :: String
serverUrl = "http://localhost:" ++ (show serverPort) ++ "/"

data ChatMessage = ChatMessage {
  -- userID :: Int,
  username :: String,
  message :: String,
  time :: UTCTime
}

instance FromJSON ChatMessage where
  parseJSON = withObject "chatMessage" $ \o ->
    ChatMessage
      -- <$> o .: "userID"
      <$> o .: "username"
      <*> o .: "message"
      <*> o .: "time"

instance ToJSON ChatMessage where
  toJSON p = object [
    --"userID" .= userID p,
    "username"  .= username p,
    "message"  .= message p,
    "time"  .= time p
    ]


