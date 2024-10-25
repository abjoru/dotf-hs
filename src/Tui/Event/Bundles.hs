module Tui.Event.Bundles (bundlesEvent) where

import           Brick               (get, zoom)
import           Brick.Widgets.List  (handleListEvent, handleListEventVi,
                                      listSelectedElement, listSelectedL)
import           Control.Monad.State (MonadIO (liftIO))
import           Dotf.Utils          (resolveBundleFile, resolveScript')
import           Graphics.Vty        (Event (EvKey), Key (KChar, KEsc))
import           Lens.Micro.Mtl      (use, (.=))
import           Tui.State           (DEvent, Focus (..), bundleSelFile,
                                      bundlesL, focusL, gitPackagesL,
                                      maybeEditFile, newBundleL, packagesL,
                                      scriptsL, selectBundle, syncBundles)

--------------------
-- Event Handlers --
--------------------

bundlesEvent :: Focus -> Event -> DEvent ()
bundlesEvent _ (EvKey (KChar 'n') []) = doCreateNewBundle
bundlesEvent FBundleList ev           = bundleListEvent ev
bundlesEvent FPackageList ev          = packageListEvent ev
bundlesEvent FGitPackageList ev       = gitListEvent ev
bundlesEvent FScriptList ev           = scriptListEvent ev
bundlesEvent _ _                      = return ()

bundleListEvent :: Event -> DEvent ()
bundleListEvent (EvKey (KChar 'e') []) = doEditBundle
bundleListEvent (EvKey KEsc [])        = doUnselect
bundleListEvent ev                     = doMove ev

packageListEvent :: Event -> DEvent ()
packageListEvent (EvKey KEsc []) = zoom packagesL $ listSelectedL .= Nothing
packageListEvent ev = zoom packagesL $ handleListEventVi handleListEvent ev

gitListEvent :: Event -> DEvent ()
gitListEvent (EvKey KEsc []) = zoom gitPackagesL $ listSelectedL .= Nothing
gitListEvent ev = zoom gitPackagesL $ handleListEventVi handleListEvent ev

scriptListEvent :: Event -> DEvent ()
scriptListEvent (EvKey (KChar 'e') []) = doEditScript
scriptListEvent (EvKey KEsc []) = zoom scriptsL $ listSelectedL .= Nothing
scriptListEvent ev = zoom scriptsL $ handleListEventVi handleListEvent ev

-------------
-- Actions --
-------------

doCreateNewBundle :: DEvent ()
doCreateNewBundle = do
  focusL     .= FNewBundleEditor
  newBundleL .= True

doMove :: Event -> DEvent ()
doMove ev = do
  zoom bundlesL $ handleListEventVi handleListEvent ev
  lst <- use bundlesL
  selectBundle $ fmap snd (listSelectedElement lst)

doUnselect :: DEvent ()
doUnselect = do
  zoom bundlesL $ listSelectedL .= Nothing
  selectBundle Nothing

doEditBundle :: DEvent ()
doEditBundle = do
  state <- get
  file  <- liftIO $ resolveBundleFile (bundleSelFile state)
  maybeEditFile file >> syncBundles

doEditScript :: DEvent ()
doEditScript = do
  script <- use scriptsL
  file   <- liftIO $ resolveScript' (snd <$> listSelectedElement script)
  maybeEditFile file
