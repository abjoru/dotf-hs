module Tui (tui) where

import           Brick         (App (..), CursorLocation, defaultMain,
                                showCursorNamed)
import           Brick.Themes  (themeToAttrMap)
import           Control.Monad (void)
import           Dotf.Bundles  (loadBundles)
import           Dotf.Commands (listTracked, listUntracked)
import           Dotf.Types    (GitError, TrackedType)
import           Lens.Micro    ((^.))
import           Tui.Events    (handleEvents)
import           Tui.State     (Focus (FIgnoreEditor, FNewBundleEditor),
                                RName (RIgnoreEditor, RNewBundleEditor), State,
                                focusL, withState)
import           Tui.Theme     (theme)
import           Tui.Widgets   (ui)

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
  FIgnoreEditor    -> showCursorNamed RIgnoreEditor r
  FNewBundleEditor -> showCursorNamed RNewBundleEditor r
  _                -> Nothing

loadTracked :: IO [TrackedType]
loadTracked = resultOrDie <$> listTracked

loadUntracked :: IO [FilePath]
loadUntracked = resultOrDie <$> listUntracked

resultOrDie :: Either GitError [a] -> [a]
resultOrDie (Right v) = v
resultOrDie (Left er) = error $ show er

tui :: IO ()
tui = do
  state <- withState <$> loadTracked
                     <*> loadUntracked
                     <*> loadBundles
  void $ defaultMain app state
