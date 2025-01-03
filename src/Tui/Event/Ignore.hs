module Tui.Event.Ignore (editIgnoreEvent) where

import           Brick               (BrickEvent (VtyEvent))
import           Brick.Widgets.Edit  (editContentsL, handleEditorEvent)
import           Control.Monad.State (MonadIO (liftIO))
import           Data.Text.Zipper    (getText)
import           Graphics.Vty        (Event (EvKey), Key (KEnter, KEsc))
import           Lens.Micro          ((^.))
import           Lens.Micro.Mtl      (use, zoom, (.=))

import qualified Dotf.Commands       as CMD
import           Tui.State           (DEvent, Focus (FUntracked), RName, focusL,
                                      ignoreEditL, ignoreL, syncUntracked)

--------------------
-- Event Handlers --
--------------------

editIgnoreEvent :: BrickEvent RName e -> DEvent ()
editIgnoreEvent (VtyEvent (EvKey KEsc [])) = doExitIgnore
editIgnoreEvent (VtyEvent (EvKey KEnter [])) = doSubmitIgnore
editIgnoreEvent ev = zoom ignoreEditL $ handleEditorEvent ev

-------------
-- Actions --
-------------

doExitIgnore :: DEvent ()
doExitIgnore = do
  focusL  .= FUntracked
  ignoreL .= False

doSubmitIgnore :: DEvent ()
doSubmitIgnore = do
  ed <- use ignoreEditL
  let content = ed ^. editContentsL
  liftIO $ CMD.ignoreFile (head $ getText content)
  syncUntracked >> doExitIgnore
