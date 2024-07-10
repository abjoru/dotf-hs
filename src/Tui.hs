module Tui where

import qualified Brick              as M
import           Brick.Themes       (themeToAttrMap)
import qualified Brick.Widgets.List as L

import           Control.Monad      (void)

import qualified Graphics.Vty       as V

import           Lens.Micro.Mtl     (use, (.=))
import           Tui.State          (Focus (FTracked, FUntracked), RName,
                                     State (_focus), Tab (..), focusL,
                                     sampleState, tabL, trackedL, untrackedL)
import           Tui.Theme          (theme)
import           Tui.Widgets        (ui)

appEvent :: M.BrickEvent RName e -> M.EventM RName State ()
appEvent (M.VtyEvent (V.EvKey V.KEsc []))         = M.halt
appEvent (M.VtyEvent (V.EvKey (V.KChar '1') []))  = switchTab DotfileTab
appEvent (M.VtyEvent (V.EvKey (V.KChar '2') []))  = switchTab BundleTab
appEvent (M.VtyEvent (V.EvKey (V.KChar '\t') [])) = toggleFocus
appEvent (M.VtyEvent ev)                          = handleListEvents ev
appEvent _                                        = return ()

switchTab :: Tab -> M.EventM RName State ()
switchTab DotfileTab = do
  tabL .= DotfileTab
  focusL .= FTracked
switchTab t = tabL .= t

toggleFocus :: M.EventM RName State ()
toggleFocus = do
  tab <- use tabL
  focus <- use focusL
  case (tab, focus) of
    (DotfileTab, FTracked)   -> focusL .= FUntracked
    (DotfileTab, FUntracked) -> focusL .= FTracked
    _                        -> return ()

handleListEvents :: V.Event -> M.EventM RName State ()
handleListEvents ev = use focusL >>= handle
  where handle FTracked = M.zoom trackedL $ L.handleListEventVi L.handleListEvent ev
        handle FUntracked = M.zoom untrackedL $ L.handleListEventVi L.handleListEvent ev
        handle _ = return ()

app :: M.App State e RName
app = M.App { M.appDraw = ui
            , M.appChooseCursor = M.neverShowCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return ()
            , M.appAttrMap = const $ themeToAttrMap theme
            }

tui :: IO ()
tui = void $ M.defaultMain app sampleState { _focus = FUntracked }
