module Tui where

import qualified Brick                 as M
import           Brick.Themes          (themeToAttrMap)

import           Control.Monad         (void)

import qualified Brick.Widgets.List    as L
import           Control.Dotf.Commands (listTracked, listUntracked)
import           Data.Dotf             (TrackedType)
import qualified Data.Vector           as V
import           Tui.Events            (appEvent)
import           Tui.State             (RName (..),
                                        State (_tracked, _untracked),
                                        emptyState)
import           Tui.Theme             (theme)
import           Tui.Widgets           (ui)

app :: M.App State e RName
app = M.App { M.appDraw = ui
            , M.appChooseCursor = M.neverShowCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return ()
            , M.appAttrMap = const $ themeToAttrMap theme
            }

initState :: IO State
initState = do
  tracked <- tryListTracked
  untracked <- tryListUntracked
  return $ emptyState { _tracked = L.list RTrackedList (V.fromList tracked) 1
                      , _untracked = L.list RUntrackedList (V.fromList untracked) 1
                      }

tryListTracked :: IO [TrackedType]
tryListTracked = do
  xs <- listTracked
  return $ case xs of
    Right v -> v
    Left er -> error $ show er

tryListUntracked :: IO [FilePath]
tryListUntracked = do
  xs <- listUntracked
  return $ case xs of
    Right v -> v
    Left er -> error $ show er

tui :: IO ()
tui = do
  state <- initState
  void $ M.defaultMain app state
