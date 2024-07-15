module Tui.Event.Dotfiles (dotfilesEvent) where

import           Brick                 (suspendAndResume)
import           Brick.Widgets.Edit    (editContentsL)
import           Brick.Widgets.List    (handleListEvent, handleListEventVi,
                                        list, listSelectedL)
import           Control.Dotf.Commands (stageFile, unsafeListTracked,
                                        unstageFile, untrackFile)
import           Control.Monad.State   (MonadIO (liftIO), get)
import           Data.Dotf             (TrackedType (..))
import           Data.Text.Zipper      (stringZipper)
import qualified Data.Vector           as V
import           Graphics.Vty          (Event (EvKey), Key (KChar, KEsc))
import           Lens.Micro.Mtl        (use, zoom, (.=))
import           System.Directory      (getHomeDirectory)
import           System.FilePath       ((</>))
import           System.Process        (callProcess)
import           Tui.State

--------------------
-- Event Handlers --
--------------------

dotfilesEvent :: Focus -> Event -> DEvent ()
dotfilesEvent _ (EvKey (KChar 'a') [])          = doShowToggle
dotfilesEvent f (EvKey KEsc [])                 = doUnselect f
dotfilesEvent FTracked (EvKey (KChar 'e') [])   = doEditTracked
dotfilesEvent FTracked (EvKey (KChar 'l') [])   = doFocusRight
dotfilesEvent FTracked (EvKey (KChar 'R') [])   = doRemoveFile
dotfilesEvent FTracked (EvKey (KChar 'A') [])   = doAddFile
dotfilesEvent FTracked ev                       = trackedListEvent ev
dotfilesEvent FUntracked (EvKey (KChar 'e') []) = doEditUntracked
dotfilesEvent FUntracked (EvKey (KChar 'h') []) = doFocusLeft
dotfilesEvent FUntracked (EvKey (KChar 'A') []) = doTrackFile
dotfilesEvent FUntracked (EvKey (KChar 'I') []) = doIgnoreFile
dotfilesEvent FUntracked ev                     = untrackedListEvent ev
dotfilesEvent _ _                               = return ()

trackedListEvent :: Event -> DEvent ()
trackedListEvent ev = zoom trackedL $ handleListEventVi handleListEvent ev

untrackedListEvent :: Event -> DEvent ()
untrackedListEvent ev = zoom untrackedL $ handleListEventVi handleListEvent ev

-------------
-- Actions --
-------------

doUnselect :: Focus -> DEvent ()
doUnselect FTracked   = zoom trackedL $ listSelectedL .= Nothing
doUnselect FUntracked = zoom untrackedL $ listSelectedL .= Nothing
doUnselect _          = return ()

doShowToggle :: DEvent ()
doShowToggle = do
  s <- use showAllTrackedL
  r <- liftIO $ do
    rs <- unsafeListTracked (not s)
    return $ list RTrackedList (V.fromList rs) 1
  trackedL .= r
  showAllTrackedL .= not s

doEditTracked :: DEvent ()
doEditTracked = get >>= (\s -> byFile s $ trackedSelFilePath s)
  where byFile s (Just fp) = editFile fp s
        byFile _ _         = return ()

doEditUntracked :: DEvent ()
doEditUntracked = get >>= (\s -> byFile s $ untrackedSel s)
  where byFile s (Just fp) = editFile fp s
        byFile _ _         = return ()

doFocusLeft :: DEvent ()
doFocusLeft = do
  tab   <- use tabL
  focus <- use focusL
  case (tab, focus) of
    (DotfileTab, FUntracked) -> focusL .= FTracked
    _                        -> return ()

doFocusRight :: DEvent ()
doFocusRight = do
  tab   <- use tabL
  focus <- use focusL
  case (tab, focus) of
    (DotfileTab, FTracked) -> focusL .= FUntracked
    _                      -> return ()

doAddFile :: DEvent ()
doAddFile = get >>= byFile . trackedSel
  where byFile (Just (Unstaged fp True)) = liftIO (stageFile fp) >> syncDotfiles
        byFile _                         = return ()

doRemoveFile :: DEvent ()
doRemoveFile = get >>= byFile . trackedSel
  where byFile (Just (Tracked fp))    = liftIO (untrackFile fp) >> syncDotfiles
        byFile (Just (Staged fp _))   = liftIO (unstageFile fp) >> syncDotfiles
        byFile (Just (Unstaged fp _)) = liftIO (untrackFile fp) >> syncTracked
        byFile _                      = return ()

doTrackFile :: DEvent ()
doTrackFile = get >>= byFile . untrackedSel
  where byFile (Just fp) = liftIO (stageFile fp) >> syncDotfiles
        byFile _         = return ()

doIgnoreFile :: DEvent ()
doIgnoreFile = get >>= setIgnoreFile . untrackedSel

-----------
-- Utils --
-----------

editFile :: FilePath -> State -> DEvent ()
editFile fp s = suspendAndResume $ do
  home <- getHomeDirectory
  callProcess "nvim" [home </> fp] >> pure s

setIgnoreFile :: Maybe FilePath -> DEvent ()
setIgnoreFile Nothing = return ()
setIgnoreFile (Just fp) = do
  focusL  .= FIgnoreEditor
  ignoreL .= True
  zoom ignoreEditL $ editContentsL .= stringZipper [fp] Nothing
