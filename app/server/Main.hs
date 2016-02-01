{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import qualified Web.Scotty as W
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import Control.Monad.IO.Class  (liftIO)
import Control.Monad
import Database.Persist
import qualified Database.Persist.Sqlite as DB
import Database.Persist.TH
import Data.Time.Clock
import Data.Text as T
import Data.Text.Lazy as TL
import Data.Int
import Data.Hash.MD5
import Network.HTTP.Types.Status
import Data.Maybe
import Network.Wai.Internal
import Data.List as L
import Data.List as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Base64
import Network.HTTP.Types.Header
import Data.Text.Encoding
import Lib
import Data.Aeson as JSON
import Data.Aeson.Extra as JsonExtra

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    login String
		password String
    deriving Show
Post
    text String
    authorId UserId
		time UTCTime default=CURRENT_TIME
    deriving Show
|]

app = do
  W.get "/favicon.ico" $ W.html "ಠ_ಠ"

  W.get "/:some" $ do
    r <- W.request
    authorized <- liftIO $ auth r
    when authorized W.next
    liftIO $ print $ requestHeaders r
    W.status status401
    W.addHeader "WWW-Authenticate" "Basic realm=\"Incoming\""
    W.html $ renderHtml $ H.html $ do H.h2 "Authentication required."

  W.get "/sendMessage/:id/:text" $ do
    userId :: Int <- W.param "id"
    msgText :: String <- W.param "text"
    liftIO $ addPost userId msgText
    W.text $ TL.pack $ "Post with text '" ++ msgText ++ "' added"

  W.get "/addUser/:login/:pass" $ do
    login :: String <- W.param "login"
    pass :: String <- W.param "pass"
    key <- liftIO $ addUser login pass
    W.text $ TL.pack $ show $ DB.fromSqlKey key

  W.get "/checkCreds/:login/:pass" $ do
    login :: String <- W.param "login"
    pass :: String <- W.param "pass"
    res <- liftIO $ verifyCredentials login pass
    W.text $ TL.pack $ show $ res

  W.get "/getLast/:num" $ do
    numberOfPosts :: Int <- W.param "num"
    lastPosts <- liftIO $ getLastMessages numberOfPosts
    W.json lastPosts

  W.get "/getSince/:time" $ do
      timeStr :: T.Text <- W.param "time"
      let time :: Maybe UTCTime = JsonExtra.decode $ BSL.fromStrict $ encodeUtf8 $ timeStr
      lastPosts <- liftIO $ getMessagesSince (fromJust time)
      W.json lastPosts

auth :: Request -> IO (Bool)
auth (Request {requestHeaders=h}) = do
  let userAndPass = L.find (\(x,y) -> x == hAuthorization) h
  case userAndPass of
    Nothing -> return False
    Just (hn, hv) -> do
      let
         decodedStr = decodeUtf8 $ decodeLenient $ L.head $ L.drop 1 $ BS.split ' ' hv
         username:pass:xs = T.unpack <$> T.split (==':') decodedStr
      usr <- verifyCredentials username pass
      return $ isJust usr

addPost :: Int -> String -> IO(Key Post)
addPost id msg = DB.runSqlite ":embedded:" $ do
  let userID = fromIntegral(id) :: Int64
      userKey :: Key User = DB.toSqlKey userID
  curTime <- liftIO $ getCurrentTime
  user <- DB.get userKey
  DB.insert $ Post msg (DB.toSqlKey userID) curTime

addUser :: String -> String -> IO(Key User)
addUser login pass = DB.runSqlite ":embedded:" $ do
  entity <- DB.insert $ User login (uberHash pass)
  return entity

getLastMessages :: Int -> IO([ChatMessage])
getLastMessages no = DB.runSqlite ":embedded:" $ do
  let numberOfPosts = fromIntegral no :: Int64
  lastPosts <- E.select
           $ E.from $ \(post `E.InnerJoin` user) -> do
             E.on $ post ^. PostAuthorId E.==. user ^. UserId
             E.orderBy [E.desc (post ^. PostTime)]
             E.limit numberOfPosts
             return (user ^. UserLogin, post ^. PostText, post ^. PostTime)
  let toMsg (login, msg, time) = ChatMessage (E.unValue login) (E.unValue msg) (E.unValue time)
  return $ toMsg <$> lastPosts

getMessagesSince :: UTCTime -> IO[ChatMessage]
getMessagesSince sinceTime = DB.runSqlite ":embedded:" $ do
  let oneSecond = secondsToDiffTime 1
      oneSecondDiff :: NominalDiffTime = fromRational $ toRational $ oneSecond
      realSinceTime = addUTCTime oneSecondDiff sinceTime
  lastPosts <- E.select
           $ E.from $ \(post `E.InnerJoin` user) -> do
             E.where_ (post ^. PostTime E.>. E.val realSinceTime)
             E.on $ post ^. PostAuthorId E.==. user ^. UserId
             E.orderBy [E.desc (post ^. PostTime)]
             return (user ^. UserLogin, post ^. PostText, post ^. PostTime)
  let toMsg (login, msg, time) = ChatMessage (E.unValue login) (E.unValue msg) (E.unValue time)
      result = toMsg <$> lastPosts
  return result


verifyCredentials :: String -> String -> IO(Maybe Int64)
verifyCredentials username expectedPass = DB.runSqlite ":embedded:" $ do
  userKeys <- selectKeysList [UserLogin ==. username] [LimitTo 1]
  users <-  selectList [UserLogin ==. username] [LimitTo 1]
  let actualPass = userPassword $ entityVal (L.head users)
  return $
    if (1 /= Prelude.length users) then Nothing
    else if actualPass == uberHash expectedPass then Just (DB.fromSqlKey (L.head userKeys)) else Nothing

main :: IO ()
main = W.scotty serverPort app
