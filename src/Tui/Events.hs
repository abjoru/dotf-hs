module Tui.Events (
  DEvent,
  handleEvents,
  syncUntracked
) where

import           Brick              (BrickEvent (VtyEvent), halt)
import           Brick.Widgets.List (listSelectedL)
import           Graphics.Vty       (Event (EvKey), Key (KChar))
import           Lens.Micro.Mtl     (use, zoom, (.=))
import           Tui.Event.Bundles  (bundlesEvent)
import           Tui.Event.Dotfiles (dotfilesEvent)
import           Tui.Event.Ignore   (editIgnoreEvent)
import           Tui.State

--------------------
-- Event Handlers --
--------------------

handleEvents :: BrickEvent RName e -> DEvent ()
handleEvents ev@(VtyEvent e) = do
  tab    <- use tabL
  ignore <- use ignoreL
  case (tab, ignore) of
    (DotfileTab, True) -> editIgnoreEvent ev
    _                  -> appEvent e
handleEvents _ = return ()

appEvent :: Event -> DEvent ()
appEvent (EvKey (KChar 'q') [])  = halt
appEvent (EvKey (KChar '1') [])  = doSwitchTab DotfileTab
appEvent (EvKey (KChar '2') [])  = doSwitchTab BundleTab
appEvent (EvKey (KChar '\t') []) = doToggleFocus
appEvent ev = do
  tab    <- use tabL
  focus  <- use focusL
  case tab of
    DotfileTab -> dotfilesEvent focus ev
    BundleTab  -> bundlesEvent focus ev

-------------
-- Actions --
-------------

doSwitchTab :: Tab -> DEvent ()
doSwitchTab DotfileTab = do
  tabL   .= DotfileTab
  focusL .= FTracked
doSwitchTab BundleTab = do
  tabL   .= BundleTab
  focusL .= FBundleList
  zoom bundlesL $ listSelectedL .= Nothing

doToggleFocus :: DEvent ()
doToggleFocus = do
  tab   <- use tabL
  focus <- use focusL
  case (tab, focus) of
    (DotfileTab, FTracked)   -> focusL .= FUntracked
    (DotfileTab, FUntracked) -> focusL .= FTracked
    _                        -> return ()
