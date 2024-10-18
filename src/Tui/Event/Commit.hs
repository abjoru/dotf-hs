module Tui.Event.Commit (editCommitEvent) where

import           Brick               (BrickEvent (VtyEvent))
import           Brick.Widgets.Edit  (editContentsL, handleEditorEvent)
import           Control.Monad.State (MonadIO (liftIO))
import           Data.Text.Zipper    (getText)
import qualified Dotf.Commands       as CMD
import           Graphics.Vty        (Event (EvKey), Key (KEnter, KEsc))
import           Lens.Micro          ((^.))
import           Lens.Micro.Mtl      (use, zoom, (.=))
import           Tui.State           (DEvent, RName, commitEditL, commitL,
                                      syncTracked)

--------------------
-- Event Handlers --
--------------------

editCommitEvent :: BrickEvent RName e -> DEvent ()
editCommitEvent (VtyEvent (EvKey KEsc []))   = doExitCommit
editCommitEvent (VtyEvent (EvKey KEnter [])) = doSubmitCommit
editCommitEvent ev = zoom commitEditL $ handleEditorEvent ev

-------------
-- Actions --
-------------

doExitCommit :: DEvent()
doExitCommit = commitL .= False

doSubmitCommit :: DEvent ()
doSubmitCommit = do
  ed <- use commitEditL
  let content = ed ^. editContentsL
  liftIO $ CMD.commit' (head $ getText content)
  syncTracked >> doExitCommit
