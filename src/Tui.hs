module Tui (tui) where

import           Brick                (App (..), CursorLocation, Widget,
                                       defaultMain, showCursorNamed)
import           Brick.Themes         (themeToAttrMap)
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core   (padAll, str)
import qualified Brick.Widgets.Dialog as D
import           Control.Monad        (void)
import           Dotf.Bundles         (loadBundles)
import qualified Dotf.Commands        as CMD
import           Dotf.XMonad          (loadAppConfig)
import           Lens.Micro           ((^.))
import           Tui.Events           (handleEvents)
import           Tui.State            (Focus (FCommitEditor, FFilterEditor, FIgnoreEditor, FNewBundleEditor),
                                       RName (RCommitEditor, RFilterEditor, RIgnoreEditor, RNewBundleEditor),
                                       State (_error, _popup, _tab),
                                       Tab (BundleTab, DotfileTab, XMonadTab),
                                       focusL, withState')
import           Tui.Tab.Bundles      (bundleTab)
import           Tui.Tab.Dotfiles     (dotfileTab)
import           Tui.Tab.XMonad       (xmonadTab)
import           Tui.Theme            (theme)

app :: App State e RName
app = App {
  appDraw         = appUi,
  appChooseCursor = appCursor,
  appHandleEvent  = handleEvents,
  appStartEvent   = return (),
  appAttrMap      = const $ themeToAttrMap theme
}

appUi :: State -> [Widget RName]
appUi st = maybe [drawUi $ _tab st] (\p -> [drawDialog (_popup st) p, drawUi $ _tab st]) (_error st)
  where drawUi DotfileTab = dotfileTab st
        drawUi BundleTab  = bundleTab st
        drawUi XMonadTab  = xmonadTab st
        drawDialog d ln = D.renderDialog d $ C.hCenter $ padAll 1 $ str $ unlines ln

appCursor :: State -> [CursorLocation RName] -> Maybe (CursorLocation RName)
appCursor state r = case state ^. focusL of
  FIgnoreEditor    -> showCursorNamed RIgnoreEditor r
  FNewBundleEditor -> showCursorNamed RNewBundleEditor r
  FFilterEditor    -> showCursorNamed RFilterEditor r
  FCommitEditor    -> showCursorNamed RCommitEditor r
  _                -> Nothing

tui :: IO ()
tui = do
  state <- withState' <$> CMD.listTracked
                      <*> CMD.listUntracked
                      <*> loadBundles
                      <*> loadAppConfig
  void $ defaultMain app state
