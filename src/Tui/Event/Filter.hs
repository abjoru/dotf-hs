module Tui.Event.Filter (editFilterEvent) where

import Brick
import Brick.Widgets.Edit
import Graphics.Vty
import Lens.Micro.Mtl
import Tui.State

--------------------
-- Event Handlers --
--------------------

editFilterEvent :: BrickEvent RName e -> DEvent ()
editFilterEvent (VtyEvent (EvKey KEsc [])) = doExitFilter
editFilterEvent (VtyEvent (EvKey KEnter [])) = doSubmitFilter
editFilterEvent ev = zoom filterEditL $ handleEditorEvent ev

-------------
-- Actions --
-------------

doExitFilter :: DEvent ()
doExitFilter = do
  focusL .= FUntracked
  filterL .= False

doSubmitFilter :: DEvent ()
doSubmitFilter = do
  filterL .= False
  syncDotfiles
