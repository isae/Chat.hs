{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Monad.State as D
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Model
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State as S
import Control.Monad
import Network
import System.Environment
import System.Exit
import System.IO
import Data.Typeable
import Data.Time
import Lib
import qualified Data.HashTable.IO as H
import Data.Aeson as JSON
import Data.Aeson.Extra as JsonExtra
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)

import Network.HTTP.Conduit -- the main module

-- The streaming interface uses conduits
import Data.Conduit
import Data.Conduit.Binary (sinkFile)

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 hiding (length, head, null)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

import Data.IORef

data ChatStatus = Unregistered | Ok deriving (Show, Eq)

prettyPrint :: ChatMessage -> String
prettyPrint msg  = (username msg) ++ (" wrote: "::String) ++ (message msg) ++ (show $ time msg) -- todo replace with lenses maybe

updateList :: IORef UTCTime -> ListStore String -> IO()
updateList tm list = do
  lastTime <- readIORef tm
  print $ JSON.encode lastTime
  System.IO.putStrLn $ "Encoded version: " ++ (unpack $ JSON.encode lastTime)
  let url = serverUrl ++ ("getSince/"::String) ++ (unpack $ JSON.encode lastTime)
  print url
  ans <- simpleHttp $ url
  let messages =  JSON.decode ans :: Maybe [ChatMessage]
  when (isJust messages && length (fromJust messages) > 1) ( do
      let jm = fromJust messages
      writeIORef tm $ time $ (head jm)
      let strings = Prelude.reverse <$> Prelude.map prettyPrint $ jm
      mapM_ (listStoreAppend list) strings
    )
  return ()


main :: IO ()
main = do
  --Read saved notes
  curStatus <- newIORef Unregistered
  lastTime <- newIORef utcStart
  userIDRef <- newIORef Nothing
  list2 <- listStoreNew ["Please, register or sign in to join chat"]
  ans <- simpleHttp $ serverUrl ++ ("getLast/5"::String)
  let messages =  JSON.decode ans :: Maybe [ChatMessage]
  when (isJust messages) ( do
      let jm = fromJust messages
      lastTtm <- newIORef $ time $ (head jm)
      let strings = Prelude.map prettyPrint $ jm
      mapM_ (listStoreAppend list2) strings
      return ()
    )
  initGUI
  window <- windowNew
  set window [windowTitle := ("Chat client"::String), windowDefaultWidth := 500, windowDefaultHeight := 400]

  --mainBox
  mainBox <- vBoxNew False 1
  containerAdd window mainBox

  --usernameBox
  usernameLbl <- labelNew(Just ("Login"::String))
  usernameBox <- hBoxNew False 1
  usernameEdt <- entryNew
  registerBtn <- buttonNewWithLabel ("Register"::String)
  boxPackStart usernameBox usernameLbl PackNatural 1
  boxPackStart usernameBox usernameEdt PackGrow 0
  boxPackStart usernameBox registerBtn PackNatural 0

  passwordLbl <- labelNew(Just ("Password"::String))
  passwordBox <- hBoxNew False 1
  passwordEdt <- entryNew
  entrySetVisibility passwordEdt False
  signInBtn <- buttonNewWithLabel ("Sigh in"::String)
  boxPackStart passwordBox passwordLbl PackNatural 0
  boxPackStart passwordBox passwordEdt PackGrow 1
  boxPackStart passwordBox signInBtn PackNatural 0

  --addBox
  addBox <- hBoxNew False 1
  addEdt <- entryNew
  addBtn <- buttonNewWithLabel ("Send!"::String)
  updateBtn <- buttonNewWithLabel ("Update"::String)
  boxPackStart addBox addEdt PackGrow 0
  boxPackStart addBox addBtn PackNatural 0
  boxPackStart addBox updateBtn PackNatural 1

  --init TreeView
  treeview <- treeViewNewWithModel list2
  treeViewSetHeadersVisible treeview False
  col <- treeViewColumnNew
  rend <- cellRendererTextNew
  cellLayoutPackStart col rend False
  cellLayoutSetAttributes col rend list2 (\i -> [cellText := i])
  treeViewAppendColumn treeview col

  -- set selection
  selection <- treeViewGetSelection treeview
  treeSelectionSetMode selection SelectionSingle

  lbl <- labelNew(Just ("Messages"::String))
  boxPackStart mainBox usernameBox PackNatural 0
  boxPackStart mainBox passwordBox PackNatural 1
  boxPackStart mainBox lbl PackNatural 0
  boxPackStart mainBox treeview PackGrow 0
  boxPackStart mainBox addBox PackNatural 0
  set window [windowTitle := ("Chat"::String), windowDefaultWidth := 350, windowDefaultHeight := 800, containerBorderWidth := 30]
  --S.execStateT (addButtonHandlers addBtn addEdt list2) initialState
  on addBtn buttonActivated $ do
    status <- readIORef curStatus
    curText :: String <- entryGetText addEdt
    if status == Unregistered
      then do
        listStoreAppend list2 "You must register or log in first!"
        return ()
      else
        if (length curText /= 0)
          then do
            curTime <- readIORef lastTime
            liftIO $ print curTime
            user <- readIORef userIDRef
            let userID = fromJust user
            simpleHttp $ serverUrl ++ ("sendMessage/"::String) ++ (show userID) ++ "/" ++ curText
            updateList lastTime list2
            return ()
          else
           return ()

  on signInBtn buttonActivated $ do
    status <- readIORef curStatus
    curText :: String <- entryGetText addEdt
    if status == Unregistered
       then do
        username <- entryGetText usernameEdt
        password <- entryGetText passwordEdt
        ans <- simpleHttp $ serverUrl ++ ("checkCreds/"::String) ++ username ++ "/" ++ password
        let res :: Maybe Int  = read $ unpack ans
        if isJust res
           then do
             listStoreAppend list2 "You successfully signed in"
             writeIORef curStatus Ok
             writeIORef userIDRef res
             return ()
           else listStoreAppend list2 "Incorrect login or password" >> return ()
       else do
        listStoreAppend list2 "You have already signed in"
        return ()

  on updateBtn buttonActivated $ do
    updateList lastTime list2

  on registerBtn buttonActivated $ do
    status <- readIORef curStatus
    curText :: String <- entryGetText addEdt
    if status == Unregistered
       then do
        username <- entryGetText usernameEdt
        password <- entryGetText passwordEdt
        ans <- simpleHttp $ serverUrl ++ ("addUser/"::String) ++ username ++ "/" ++ password
        let res :: Maybe Int  = readMaybe $ unpack ans
        if isJust res
           then do
             listStoreAppend list2 "Successful registration"
             writeIORef curStatus Ok
             writeIORef userIDRef res
             return ()
           else listStoreAppend list2 "Username already exists" >> return ()
       else do
        listStoreAppend list2 "You have already signed in"
        return ()

  widgetShowAll window
  mainGUI
