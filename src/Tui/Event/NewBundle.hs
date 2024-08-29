module Tui.Event.NewBundle (newBundleEvent) where

import Brick (BrickEvent (VtyEvent))
import Brick.Widgets.Edit (editContentsL, handleEditorEvent)
import Control.Dotf.Commands (newBundle)
import Control.Monad.State (MonadIO (liftIO))
import Data.Text.Zipper (getText)
import Graphics.Vty (Event (EvKey), Key (KEnter, KEsc))
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, zoom, (.=))

import Tui.State

--------------------
-- Event Handlers --
--------------------

newBundleEvent :: BrickEvent RName e -> DEvent ()
newBundleEvent (VtyEvent (EvKey KEsc [])) = doExitNewBundle
newBundleEvent (VtyEvent (EvKey KEnter [])) = doSubmitNewBundle
newBundleEvent ev = zoom newBundleEditL $ handleEditorEvent ev

-------------
-- Actions --
-------------

doExitNewBundle :: DEvent ()
doExitNewBundle = do
  focusL .= FBundleList
  newBundleL .= False

doSubmitNewBundle :: DEvent ()
doSubmitNewBundle = do
  ed <- use newBundleEditL
  let content = ed ^. editContentsL
  liftIO $ newBundle (head $ getText content)
  syncBundles >> doExitNewBundle
