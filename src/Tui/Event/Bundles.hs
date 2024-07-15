module Tui.Event.Bundles (bundlesEvent) where

import           Brick
import           Brick.Widgets.List
import           Graphics.Vty
import           Lens.Micro.Mtl
import           Tui.State

bundlesEvent :: Focus -> Event -> DEvent ()
bundlesEvent FBundleList ev = bundleListEvent ev
bundlesEvent _ _            = return ()

bundleListEvent :: Event -> DEvent ()
bundleListEvent (EvKey KEsc []) = doUnselect
bundleListEvent ev = doMove ev

-------------
-- Actions --
-------------

doMove :: Event -> DEvent ()
doMove ev = do
  zoom bundlesL $ handleListEventVi handleListEvent ev
  lst <- use bundlesL
  selectBundle $ fmap snd (listSelectedElement lst)

doUnselect :: DEvent ()
doUnselect = do
  zoom bundlesL $ listSelectedL .= Nothing
  selectBundle Nothing
