module Main where

import qualified Control.Monad.State as D
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Model
import Control.Applicative
import Control.Monad.Reader
import Control.Monad
import Data.SafeCopy
import Network
import System.Environment
import System.Exit
import System.IO
import Data.Typeable
import qualified Data.Map as Map

main :: IO()
main = do
  --Read saved notes
  {-all <- NoteList []-}
  {-listNote <- query all GetNotesList-}

  list2 <- listStoreNew ["abc", "def"]
  {-mapM_ (\(Note r)-> listStoreAppend list2 r) listNote-}

  initGUI
  window <- windowNew
  set window [windowTitle := "Notes", windowDefaultWidth := 500, windowDefaultHeight := 400]

  --mainBox
  mainBox <- vBoxNew False 1
  containerAdd window mainBox

  --addBox
  addBox <- hBoxNew False 1
  addEdt <- entryNew
  addBtn <- buttonNewWithLabel "Add"
  boxPackStart addBox addEdt PackGrow 0
  boxPackStart addBox addBtn PackNatural 0

  --editBox
  editBox <- hBoxNew False 1
  editEdt <- entryNew
  saveBtn <- buttonNewWithLabel "Save"
  boxPackStart editBox editEdt PackGrow 0
  boxPackStart editBox saveBtn PackNatural 0

  --delButton
  delBtn <- buttonNewWithLabel "Delete"

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

  lbl <- labelNew(Just "My Notes")
  boxPackStart mainBox addBox PackNatural 0
  boxPackStart mainBox editBox PackNatural 0
  boxPackStart mainBox delBtn PackNatural 0
  boxPackStart mainBox lbl PackNatural 0
  boxPackStart mainBox treeview PackGrow 0
  set window [windowTitle := "Notes", windowDefaultWidth := 300, windowDefaultHeight := 800, containerBorderWidth := 30]

  on addBtn buttonActivated $ do
    curText <- entryGetText addEdt
    if (length curText /= 0)
      then do
        listStoreAppend list2 curText
        --update all (Insert curText)
        entrySetText addEdt ""
      else
      return ()

  on delBtn buttonActivated $ do
    selRows <- treeSelectionGetSelectedRows selection
    if (null selRows)
      then
      return ()
    else do
      let index = head (head selRows)
      -- update all (DeleteByPosition index)
      listStoreRemove list2 index
      entrySetText editEdt ""
      return ()

  on selection treeSelectionSelectionChanged $ do
    selRows <- treeSelectionGetSelectedRows selection
    if (null selRows)
      then
      return ()
    else do
      let index = head (head selRows)
      v <- listStoreGetValue list2 index
      entrySetText editEdt v

  on saveBtn buttonActivated $ do
    selRows <- treeSelectionGetSelectedRows selection
    let index = head (head selRows)
    curText <- entryGetText editEdt
    if (length curText == 0)
      then
        buttonClicked delBtn
    else do
      -- update all (Edit index curText)
      listStoreSetValue list2 index curText
      entrySetText editEdt ""

  widgetShowAll window
  mainGUI
