module Tui.Events (
  DEvent,
  handleEvents,
  syncUntracked,
) where

import           Brick               (BrickEvent (VtyEvent), halt)
import           Brick.Widgets.List  (listSelectedL)
import           Graphics.Vty        (Event (EvKey),
                                      Key (KBackTab, KChar, KEnter, KEsc))
import           Lens.Micro.Mtl      (use, zoom, (.=))
import           Tui.Event.Bundles   (bundlesEvent)
import           Tui.Event.Commit    (editCommitEvent)
import           Tui.Event.Dotfiles  (dotfilesEvent)
import           Tui.Event.Filter    (editFilterEvent)
import           Tui.Event.Ignore    (editIgnoreEvent)
import           Tui.Event.NewBundle (newBundleEvent)
import           Tui.Event.XMonad    (xmonadEvent)
import           Tui.State           (DEvent, Focus (..), RName, Tab (..),
                                      bundlesL, commitL, errorL, filterL,
                                      focusL, ignoreL, newBundleL,
                                      syncUntracked, tabL)

--------------------
-- Event Handlers --
--------------------

handleEvents :: BrickEvent RName e -> DEvent ()
handleEvents ev@(VtyEvent e) = do
  tab       <- use tabL
  ignore    <- use ignoreL
  filter'   <- use filterL
  newBundle <- use newBundleL
  commit'   <- use commitL
  error'    <- use errorL
  case (tab, ignore, newBundle, filter', commit', error') of
    (_, _, _, _, _, Just _)        -> errorEvent ev
    (DotfileTab, True, _, _, _, _) -> editIgnoreEvent ev
    (BundleTab, _, True, _, _, _)  -> newBundleEvent ev
    (DotfileTab, _, _, True, _, _) -> editFilterEvent ev
    (DotfileTab, _, _, _, True, _) -> editCommitEvent ev
    _                              -> appEvent e
handleEvents _ = return ()

errorEvent :: BrickEvent RName e -> DEvent ()
errorEvent (VtyEvent (EvKey KEsc []))   = closeDialog
errorEvent (VtyEvent (EvKey KEnter [])) = closeDialog
errorEvent _                            = pure () --D.handleDialogEvent ev

appEvent :: Event -> DEvent ()
appEvent (EvKey (KChar 'q') [])  = halt
appEvent (EvKey (KChar '1') [])  = doSwitchTab DotfileTab
appEvent (EvKey (KChar '2') [])  = doSwitchTab BundleTab
appEvent (EvKey (KChar '3') [])  = doSwitchTab XMonadTab
appEvent (EvKey (KChar '\t') []) = doToggleFocus
appEvent (EvKey KBackTab [])     = doToggleReverseFocus
appEvent ev = do
  tab   <- use tabL
  focus <- use focusL
  case tab of
    DotfileTab -> dotfilesEvent focus ev
    BundleTab  -> bundlesEvent focus ev
    XMonadTab  -> xmonadEvent focus ev

-------------
-- Actions --
-------------

closeDialog :: DEvent ()
closeDialog = do
  errorL .= Nothing

doSwitchTab :: Tab -> DEvent ()
doSwitchTab DotfileTab = do
  tabL   .= DotfileTab
  focusL .= FTracked
doSwitchTab BundleTab = do
  tabL   .= BundleTab
  focusL .= FBundleList
  zoom bundlesL $ listSelectedL .= Nothing
doSwitchTab XMonadTab = do
  tabL   .= XMonadTab
  focusL .= FAppFavoritesList

doToggleFocus :: DEvent ()
doToggleFocus = do
  tab   <- use tabL
  focus <- use focusL
  case (tab, focus) of
    (DotfileTab, FTracked)         -> focusL .= FUntracked
    (DotfileTab, FUntracked)       -> focusL .= FTracked
    (BundleTab, FBundleList)       -> focusL .= FPackageList
    (BundleTab, FPackageList)      -> focusL .= FGitPackageList
    (BundleTab, FGitPackageList)   -> focusL .= FScriptList
    (BundleTab, FScriptList)       -> focusL .= FBundleList
    (XMonadTab, FAppFavoritesList) -> focusL .= FAppGamesList
    (XMonadTab, FAppGamesList)     -> focusL .= FAppInternetList
    (XMonadTab, FAppInternetList)  -> focusL .= FAppSettingsList
    (XMonadTab, FAppSettingsList)  -> focusL .= FAppSystemList
    (XMonadTab, FAppSystemList)    -> focusL .= FAppOfficeList
    (XMonadTab, FAppOfficeList)    -> focusL .= FAppFavoritesList
    _                              -> return ()

doToggleReverseFocus :: DEvent ()
doToggleReverseFocus = do
  tab   <- use tabL
  focus <- use focusL
  case (tab, focus) of
    (DotfileTab, FTracked)         -> focusL .= FUntracked
    (DotfileTab, FUntracked)       -> focusL .= FTracked
    (BundleTab, FBundleList)       -> focusL .= FScriptList
    (BundleTab, FPackageList)      -> focusL .= FBundleList
    (BundleTab, FGitPackageList)   -> focusL .= FPackageList
    (BundleTab, FScriptList)       -> focusL .= FGitPackageList
    (XMonadTab, FAppFavoritesList) -> focusL .= FAppOfficeList
    (XMonadTab, FAppGamesList)     -> focusL .= FAppFavoritesList
    (XMonadTab, FAppInternetList)  -> focusL .= FAppGamesList
    (XMonadTab, FAppSettingsList)  -> focusL .= FAppInternetList
    (XMonadTab, FAppSystemList)    -> focusL .= FAppSettingsList
    (XMonadTab, FAppOfficeList)    -> focusL .= FAppSystemList
    _                              -> return ()
