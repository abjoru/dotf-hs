module Tui (tui) where

import Brick
import Brick.Themes (themeToAttrMap)
import Control.Dotf.Commands (listTracked, listUntracked)
import Control.Monad (void)
import Data.Dotf
import Lens.Micro
import Tui.Events
import Tui.State
import Tui.Theme (theme)
import Tui.Widgets (ui)

app :: App State e RName
app =
  App
    { appDraw = ui
    , appChooseCursor = appCursor
    , appHandleEvent = handleEvents
    , appStartEvent = return ()
    , appAttrMap = const $ themeToAttrMap theme
    }

appCursor :: State -> [CursorLocation RName] -> Maybe (CursorLocation RName)
appCursor state r = case state ^. focusL of
  FIgnoreEditor -> showCursorNamed RIgnoreEditor r
  FNewBundleEditor -> showCursorNamed RNewBundleEditor r
  _ -> Nothing

loadTracked :: IO [TrackedType]
loadTracked = do
  xs <- listTracked
  return $ case xs of
    Right v -> v
    Left er -> error $ show er

loadUntracked :: IO [FilePath]
loadUntracked = do
  xs <- listUntracked
  return $ case xs of
    Right v -> v
    Left er -> error $ show er

tui :: IO ()
tui = do
  state <-
    withState
      <$> loadTracked
      <*> loadUntracked
      <*> loadBundles
  void $ defaultMain app state
