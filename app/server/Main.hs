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
import Data.ByteString.Base64
import Network.HTTP.Types.Header
import Data.Text.Encoding
import Lib
import Data.Aeson as JSON

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

greet :: String -> H.Html
greet user = H.html $ do
  H.head $
    H.title "Welcome!"
  H.body $ do
    H.h1 "Greetings!"
    H.p ("Hello " >> H.toHtml user >> "!")

app = do
  W.get "/favicon.ico" $ W.html "ಠ_ಠ"

  W.get "/:some" $ do
    r <- W.request
    authorized <- liftIO $ auth r
    when authorized W.next
    liftIO $ print $ requestHeaders r
    W.status status401
    W.addHeader "WWW-Authenticate" "Basic realm=\"Incoming\""
    W.html $ renderHtml $ H.html $ do H.h1 "Authentication required."

  W.get "/" $
    W.text "Hello!"

  W.get "/greet" $ do
    W.html $ renderHtml (greet "faggot")

  W.get "/add/:id/:text" $ do
    userId :: Int <- W.param "id"
    msgText :: String <- W.param "text"
    liftIO $ addPost userId msgText
    W.text $ TL.pack $ "Post with text '" ++ msgText ++ "' added"

  W.get "/getLast/:num" $ do
    numberOfPosts :: Int <- W.param "num"
    lastPosts <- liftIO $ getLastMessages numberOfPosts
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
      verifyCredentials username pass

addPost :: Int -> String -> IO(Key Post)
addPost id msg = DB.runSqlite ":embedded:" $ do
  let userID = fromIntegral(id) :: Int64
      userKey :: Key User = DB.toSqlKey userID
  curTime <- liftIO $ getCurrentTime
  user <- DB.get userKey
  DB.insert $ Post msg (DB.toSqlKey userID) curTime

addUser :: String -> String -> IO(User)
addUser login pass = DB.runSqlite ":embedded:" $ do
  entity <- DB.insertEntity $ User login (uberHash pass)
  return $ entityVal entity

getLastMessages :: Int -> IO([ChatMessage])
getLastMessages no = DB.runSqlite ":embedded:" $ do
  let numberOfPosts = fromIntegral(no) :: Int64
  lastPosts <- E.select
           $ E.from $ \(post `E.InnerJoin` user) -> do
             E.on $ post ^. PostAuthorId E.==. user ^. UserId
             E.orderBy [E.desc (post ^. PostTime)]
             E.limit numberOfPosts
             return (user ^. UserLogin, post ^. PostText, post ^. PostTime)
  let toMsg (login, msg, time) = ChatMessage (E.unValue login) (E.unValue msg) (E.unValue time)
  return $ L.reverse $ toMsg <$> lastPosts

verifyCredentials :: String -> String -> IO(Bool)
verifyCredentials username expectedPass = DB.runSqlite ":embedded:" $ do
  users <- selectList [UserLogin ==. username] [LimitTo 1]
  let actualPass = userPassword $ entityVal (users !! 0)
  return $
    if (1 /= (Prelude.length users)) then False
    else actualPass == (uberHash expectedPass)

uberHash :: String -> String
uberHash = md5s . Str

main :: IO ()
main = W.scotty serverPort app

app2 :: IO()
app2 = DB.runSqlite ":embedded:" $ do
    DB.runMigration migrateAll
    someUser <- liftIO $ addUser "1" "2"
    johnId <- DB.insert $ User "John Doe" $ "somePass"
    janeId <- DB.insert $ User "Jane Doe" $ "somePass2"
    curTime <- liftIO $ getCurrentTime
    DB.insert $ Post "My fr1st p0st" johnId curTime
    DB.insert $ Post "One more for good measure" johnId curTime

    oneJohnPost <- selectList [PostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity Post])

    john <- get johnId
    liftIO $ print (john :: Maybe User)

    DB.delete janeId
    -- deleteWhere [PostAuthorId ==. johnId]


abc = 5
