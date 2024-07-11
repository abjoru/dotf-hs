module Tui.Events (appEvent) where

import           Brick                 (BrickEvent (VtyEvent), EventM, halt,
                                        suspendAndResume, zoom)
import           Brick.Widgets.List    (handleListEvent, handleListEventVi,
                                        list)
import           Control.Dotf.Commands (stageFile, unsafeListTracked,
                                        unsafeListUntracked, unstageFile,
                                        untrackFile)
import           Control.Monad.State   (MonadIO (liftIO), MonadTrans (lift),
                                        get)
import           Data.Dotf             (TrackedType (..))
import qualified Data.Vector           as V
import           Graphics.Vty          (Event (EvKey), Key (KChar),
                                        Modifier (MCtrl))
import           Lens.Micro.Mtl        (use, (.=))
import           System.Directory      (getHomeDirectory)
import           System.FilePath       ((</>))
import           System.Process        (callProcess)
import           Tui.State             (Focus (FTracked, FUntracked),
                                        RName (RTrackedList, RUntrackedList),
                                        State (_focus, _showAllTracked, _tab),
                                        Tab (BundleTab, DotfileTab), focusL,
                                        showAllTrackedL, tabL, trackedL,
                                        trackedSel, trackedSelFilePath,
                                        untrackedL, untrackedSel)

type DEvent a = EventM RName State a

appEvent :: BrickEvent RName e -> DEvent ()
appEvent (VtyEvent (EvKey (KChar 'q') []))  = halt
appEvent (VtyEvent (EvKey (KChar '1') []))  = doSwitchTab DotfileTab
appEvent (VtyEvent (EvKey (KChar '2') []))  = doSwitchTab BundleTab
appEvent (VtyEvent (EvKey (KChar '\t') [])) = doToggleFocus
appEvent (VtyEvent ev) = do
  tab   <- use tabL
  focus <- use focusL
  case tab of
    DotfileTab -> dotfileTabEvent focus ev
    BundleTab  -> return ()
appEvent _ = return ()

dotfileTabEvent :: Focus -> Event -> DEvent ()
dotfileTabEvent _ (EvKey (KChar 'a') [])               = doShowToggle
dotfileTabEvent FTracked (EvKey (KChar 'e') [])        = doEditTracked
dotfileTabEvent FTracked (EvKey (KChar 'l') [MCtrl])   = doFocusRight
dotfileTabEvent FTracked (EvKey (KChar 'R') [])        = doUntrackFile
dotfileTabEvent FTracked (EvKey (KChar 'A') [])        = doAddModified
dotfileTabEvent FTracked ev                            = trackedListEvent ev
dotfileTabEvent FUntracked (EvKey (KChar 'e') [])      = doEditUntracked
dotfileTabEvent FUntracked (EvKey (KChar 'h') [MCtrl]) = doFocusLeft
dotfileTabEvent FUntracked (EvKey (KChar 'A') [])      = doTrackFile
dotfileTabEvent FUntracked ev                          = untrackedListEvent ev
dotfileTabEvent _ _                                    = return ()

trackedListEvent :: Event -> DEvent ()
trackedListEvent ev = zoom trackedL $ handleListEventVi handleListEvent ev

untrackedListEvent :: Event -> DEvent ()
untrackedListEvent ev = zoom untrackedL $ handleListEventVi handleListEvent ev

-------------
-- Actions --
-------------

doSwitchTab :: Tab -> DEvent ()
doSwitchTab DotfileTab = do
  tabL   .= DotfileTab
  focusL .= FTracked
doSwitchTab t = tabL .= t

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

doToggleFocus :: DEvent ()
doToggleFocus = do
  tab   <- use tabL
  focus <- use focusL
  case (tab, focus) of
    (DotfileTab, FTracked)   -> focusL .= FUntracked
    (DotfileTab, FUntracked) -> focusL .= FTracked
    _                        -> return ()

doEditTracked :: DEvent ()
doEditTracked = get >>= (\s -> byFile s $ trackedSelFilePath s)
  where byFile s (Just fp) = editFile fp s
        byFile _ _         = return ()

doEditUntracked :: DEvent ()
doEditUntracked = get >>= (\s -> byFile s $ untrackedSel s)
  where byFile s (Just fp) = editFile fp s
        byFile _ _         = return ()

doShowToggle :: DEvent ()
doShowToggle = do
  state  <- get
  result <- liftIO $ do
    rs   <- unsafeListTracked (not $ _showAllTracked state)
    return $ list RTrackedList (V.fromList rs) 1
  trackedL        .= result
  showAllTrackedL .= not (_showAllTracked state)

doTrackFile :: DEvent ()
doTrackFile = get >>= byFile . untrackedSel
  where byFile (Just fp) = liftIO (stageFile fp) >> syncDotfiles
        byFile _         = return ()

-- TODO this should really be git rm to remove
-- the file from being tracked altogether
doUntrackFile :: DEvent ()
doUntrackFile = get >>= byFile . trackedSel
  where byFile (Just (Tracked fp)) = liftIO (untrackFile fp) >> syncDotfiles
        byFile (Just (Staged fp))  = liftIO (unstageFile fp) >> syncDotfiles
        byFile _                   = return ()

doAddModified :: DEvent ()
doAddModified = get >>= byFile . trackedSel
  where byFile (Just (Unstaged fp)) = liftIO (stageFile fp) >> syncDotfiles
        byFile _                    = return ()

syncDotfiles :: DEvent ()
syncDotfiles = do
  state <- get
  (ts, us) <- liftIO $ loadDotfiles state
  trackedL   .= list RTrackedList (V.fromList ts) 1
  untrackedL .= list RUntrackedList (V.fromList us) 1

loadDotfiles :: State -> IO ([TrackedType], [FilePath])
loadDotfiles state = do
  tracked <- unsafeListTracked (_showAllTracked state)
  untracked <- unsafeListUntracked
  return (tracked, untracked)

editFile :: FilePath -> State -> DEvent ()
editFile fp s = suspendAndResume $ do
  home <- getHomeDirectory
  callProcess "nvim" [home </> fp] >> pure s
