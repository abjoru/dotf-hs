module Main (main) where

import           Data.Dotf                (Bundle)
import qualified Data.Yaml                as Y
import           Tui.FileBrowserDemo      (fileBrowserDemo)
import           Tui.LayerDemo            (layerDemo)
import           Tui.ListViDemo           (listViDemo)
import           Tui.SuspendAndResumeDemo (suspendAndResumeDemo)
import           Tui.TableDemo            (tableDemo)
import           Tui.ThemeDemo            (themeDemo)

main :: IO ()
main = layerDemo
--themeDemo
--tableDemo
--suspendAndResumeDemo
--fileBrowserDemo
--listViDemo

testBundleDecode :: IO ()
testBundleDecode = do
  b <- Y.decodeFileThrow "/home/abjoru/.config/dotf/bundles/shell.yaml"
  print (b :: Bundle)
