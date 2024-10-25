module Tui.Event.Filter (editFilterEvent) where

import           Brick              (BrickEvent (VtyEvent), zoom)
import           Brick.Widgets.Edit (handleEditorEvent)
import           Graphics.Vty       (Event (EvKey), Key (KEnter, KEsc))
import           Lens.Micro.Mtl     ((.=))
import           Tui.State          (DEvent, Focus (FUntracked), RName,
                                     filterEditL, filterL, focusL, syncDotfiles)

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
