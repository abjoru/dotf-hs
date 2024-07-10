module Main (main) where

import           Data.Dotf                     (Bundle)
import qualified Data.Yaml                     as Y
import           Tui                           (tui)
import           Tui.Demo.DialogDemo           (dialogDemo)
import           Tui.Demo.FileBrowserDemo      (fileBrowserDemo)
import           Tui.Demo.LayerDemo            (layerDemo)
import           Tui.Demo.ListViDemo           (listViDemo)
import           Tui.Demo.SuspendAndResumeDemo (suspendAndResumeDemo)
import           Tui.Demo.TableDemo            (tableDemo)
import           Tui.Demo.ThemeDemo            (themeDemo)

main :: IO ()
main = tui
--dialogDemo
--layerDemo
--themeDemo
--tableDemo
--suspendAndResumeDemo
--fileBrowserDemo
--listViDemo

testBundleDecode :: IO ()
testBundleDecode = do
  b <- Y.decodeFileThrow "/home/abjoru/.config/dotf/bundles/shell.yaml"
  print (b :: Bundle)
