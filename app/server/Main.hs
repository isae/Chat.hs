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
import Database.Persist
import qualified Database.Persist.Sqlite as DB
import Database.Persist.TH
import Data.Time.Clock
import Data.Text.Lazy hiding (reverse)
import Data.Int

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
  W.get "/" $
    W.text "Home Page"

  W.get "/greet/:name" $ do
    name <- W.param "name"
    W.html $ renderHtml (greet name)

  W.get "/add/:id/:text" $ do
    liftIO $ putStrLn "Hello"
    userId :: Int <- W.param "id"
    msgText :: String <- W.param "text"
    liftIO $ addPost userId msgText
    W.text $ pack $ "Post with text '" ++ msgText ++ "' added"

  W.get "/getLast/:num" $ do
    liftIO $ putStrLn "Hello2"
    numberOfPosts :: Int <- W.param "num"
    lastPosts <- liftIO $ getLastMessages numberOfPosts
    W.text $ intercalate ";\n" lastPosts

addPost :: Int -> String -> IO(Key Post)
addPost id msg = DB.runSqlite ":embedded:" $ do
  let userID = fromIntegral(id) :: Int64
      userKey :: Key User = DB.toSqlKey userID
  curTime <- liftIO $ getCurrentTime
  user <- DB.get userKey
  DB.insert $ Post msg (DB.toSqlKey userID) curTime

getLastMessages :: Int -> IO([Text])
{-getLastMessages no = DB.runSqlite ":embedded:" $ do-}
  {-lastPosts <- selectList [] [Desc PostTime, LimitTo no]-}
  {-liftIO $ print lastPosts-}
  {-return $ reverse $ pack <$> show <$> postText <$> entityVal <$> lastPosts-}

getLastMessages no = DB.runSqlite ":embedded:" $ do
  let numberOfPosts = fromIntegral(no) :: Int64
  lastPosts <- E.select
           $ E.from $ \(post `E.InnerJoin` user) -> do
             E.on $ post ^. PostAuthorId E.==. user ^. UserId
             E.orderBy [E.desc (post ^. PostTime)]
             E.limit numberOfPosts
             return (user ^. UserId, user ^. UserLogin, post ^. PostText)
  --liftIO $ print lastPosts
  return $ reverse $ pack <$> show <$> lastPosts



main :: IO ()
main = W.scotty 8000 app
app2 :: IO()
app2 = DB.runSqlite ":embedded:" $ do
    DB.runMigration migrateAll

    johnId <- DB.insert $ User "John Doe" $ "somePass"
    janeId <- DB.insert $ User "Jane Doe" $ "somePass2"
    curTime <- liftIO $ getCurrentTime
    DB.insert $ Post "My fr1st p0st" johnId curTime
    DB.insert $ Post "One more for good measure" johnId curTime

    oneJohnPost <- selectList [PostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity Post])

    john <- get johnId
    liftIO $ print (john :: Maybe User)

    delete janeId
    -- deleteWhere [PostAuthorId ==. johnId]


abc = 5
