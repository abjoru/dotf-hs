module Tui where

import qualified Brick              as M
import           Brick.Themes       (themeToAttrMap)

import           Control.Monad      (void)

import Tui.State (State, RName, sampleState)
import           Tui.Theme          (theme)
import           Tui.Widgets        (ui)
import Tui.Events (appEvent)

app :: M.App State e RName
app = M.App { M.appDraw = ui
            , M.appChooseCursor = M.neverShowCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return ()
            , M.appAttrMap = const $ themeToAttrMap theme
            }

tui :: IO ()
tui = void $ M.defaultMain app sampleState
