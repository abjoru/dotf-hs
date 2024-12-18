module Tui.Event.Dotfiles (dotfilesEvent) where

import           Brick               (get, zoom)
import           Brick.Widgets.Edit  (editContentsL)
import           Brick.Widgets.List  (handleListEvent, handleListEventVi,
                                      listSelectedL)
import           Control.Monad.State (MonadIO (liftIO))
import           Data.Text.Zipper    (stringZipper)
import qualified Dotf.Commands       as CMD
import           Dotf.Types          (TrackedType (Staged, Tracked, Unstaged))
import           Dotf.Utils          (resolveDotFile)
import           Graphics.Vty        (Event (EvKey), Key (KChar, KEsc))
import           Lens.Micro.Mtl      (use, (.=))
import           Tui.State           (DEvent,
                                      Focus (FIgnoreEditor, FTracked, FUntracked),
                                      Tab (DotfileTab), commitL, filterEditL,
                                      filterL, focusL, ignoreEditL, ignoreL,
                                      maybeDiffFile, maybeEditFile,
                                      showAllTrackedL, syncDotfiles,
                                      syncTracked, tabL, trackedL, trackedSel,
                                      trackedSelFilePath, untrackedL,
                                      untrackedSel)

--------------------
-- Event Handlers --
--------------------

dotfilesEvent :: Focus -> Event -> DEvent ()
dotfilesEvent _ (EvKey (KChar 'a') [])          = doShowToggle
dotfilesEvent _ (EvKey (KChar 'c') [])          = doCommit
dotfilesEvent f (EvKey KEsc [])                 = doUnselect f
dotfilesEvent FTracked (EvKey (KChar 'e') [])   = doEditTracked
dotfilesEvent _ (EvKey (KChar 'd') [])          = doDiff
dotfilesEvent FTracked (EvKey (KChar 'l') [])   = doFocusRight
dotfilesEvent FTracked (EvKey (KChar 'R') [])   = doRemoveFile
dotfilesEvent FTracked (EvKey (KChar 'A') [])   = doAddFile
dotfilesEvent _ (EvKey (KChar 'f') [])          = doEditFilter
dotfilesEvent _ (EvKey (KChar 'F') [])          = doClearFilter
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
  showAllTrackedL .= not s
  syncTracked

doEditTracked :: DEvent ()
doEditTracked = do
  state <- get
  file  <- liftIO $ resolveDotFile (trackedSelFilePath state)
  maybeEditFile file

doEditUntracked :: DEvent ()
doEditUntracked = do
  state <- get
  file  <- liftIO $ resolveDotFile (untrackedSel state)
  maybeEditFile file

doDiff :: DEvent ()
doDiff = do
  state <- get
  file  <- liftIO $ resolveDotFile (trackedSelFilePath state)
  maybeDiffFile file

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
 where
  byFile (Just (Unstaged fp _)) = liftIO (CMD.stageFile fp) >> syncDotfiles
  byFile _                      = return ()

doRemoveFile :: DEvent ()
doRemoveFile = get >>= byFile . trackedSel
 where
  byFile (Just (Tracked fp))    = liftIO (CMD.untrackFile fp) >> syncDotfiles
  byFile (Just (Staged fp _))   = liftIO (CMD.unstageFile fp) >> syncDotfiles
  byFile (Just (Unstaged fp _)) = liftIO (CMD.untrackFile fp) >> syncTracked
  byFile _                      = return ()

doTrackFile :: DEvent ()
doTrackFile = get >>= byFile . untrackedSel
 where
  byFile (Just fp) = liftIO (CMD.stageFile fp) >> syncDotfiles
  byFile _         = return ()

doIgnoreFile :: DEvent ()
doIgnoreFile = get >>= setIgnoreFile . untrackedSel

doEditFilter :: DEvent ()
doEditFilter = filterL .= True

-- FIXME only show if we have staged contents!
doCommit :: DEvent ()
doCommit = commitL .= True

doClearFilter :: DEvent ()
doClearFilter = do
  filterL .= False
  zoom filterEditL $ editContentsL .= stringZipper [""] Nothing
  syncDotfiles

-----------
-- Utils --
-----------

setIgnoreFile :: Maybe FilePath -> DEvent ()
setIgnoreFile Nothing = return ()
setIgnoreFile (Just fp) = do
  focusL .= FIgnoreEditor
  ignoreL .= True
  zoom ignoreEditL $ editContentsL .= stringZipper [fp] Nothing
