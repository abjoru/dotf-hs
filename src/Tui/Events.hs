module Tui.Events (appEvent) where

import Brick (BrickEvent (VtyEvent), EventM, halt, zoom)
import Tui.State (RName, State, Focus (FTracked, FUntracked), Tab (DotfileTab, BundleTab), focusL, tabL, trackedL, untrackedL)
import Graphics.Vty (Event (EvKey), Key (KChar), Modifier (MCtrl))
import Lens.Micro.Mtl (use, (.=))
import Brick.Widgets.List (handleListEventVi, handleListEvent)

type DEvent a = EventM RName State a

appEvent :: BrickEvent RName e -> DEvent ()
appEvent (VtyEvent (EvKey (KChar 'q') [])) = halt
appEvent (VtyEvent (EvKey (KChar '1') [])) = doSwitchTab DotfileTab
appEvent (VtyEvent (EvKey (KChar '2') [])) = doSwitchTab BundleTab
appEvent (VtyEvent (EvKey (KChar '\t') [])) = doToggleFocus
appEvent (VtyEvent ev) = do
  tab <- use tabL
  focus <- use focusL
  case tab of
    DotfileTab -> dotfileTabEvent focus ev
    BundleTab -> return ()
appEvent _ = return ()

dotfileTabEvent :: Focus -> Event -> DEvent ()
dotfileTabEvent FTracked (EvKey (KChar 'l') [MCtrl]) = doFocusRight
dotfileTabEvent FTracked ev = trackedListEvent ev
dotfileTabEvent FUntracked (EvKey (KChar 'h') [MCtrl]) = doFocusLeft
dotfileTabEvent FUntracked ev = untrackedListEvent ev
dotfileTabEvent _ _ = return ()

trackedListEvent :: Event -> DEvent ()
trackedListEvent ev = zoom trackedL $ handleListEventVi handleListEvent ev

untrackedListEvent :: Event -> DEvent ()
untrackedListEvent ev = zoom untrackedL $ handleListEventVi handleListEvent ev

-------------
-- Actions --
-------------

doSwitchTab :: Tab -> DEvent ()
doSwitchTab DotfileTab = do
  tabL .= DotfileTab
  focusL .= FTracked
doSwitchTab t = tabL .= t

doFocusLeft :: DEvent ()
doFocusLeft = do
  tab <- use tabL
  focus <- use focusL
  case (tab, focus) of
    (DotfileTab, FUntracked) -> focusL .= FTracked
    _ -> return ()

doFocusRight :: DEvent ()
doFocusRight = do
  tab <- use tabL
  focus <- use focusL
  case (tab, focus) of
    (DotfileTab, FTracked) -> focusL .= FUntracked
    _ -> return ()

doToggleFocus :: DEvent ()
doToggleFocus = do
  tab <- use tabL
  focus <- use focusL
  case (tab, focus) of
    (DotfileTab, FTracked) -> focusL .= FUntracked
    (DotfileTab, FUntracked) -> focusL .= FTracked
    _ -> return ()
