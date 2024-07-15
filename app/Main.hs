module Main (main) where

import           Tui (tui)

main :: IO ()
main = tui

--testBundleDecode :: IO ()
--testBundleDecode = do
  --b <- Y.decodeFileThrow "/home/abjoru/.config/dotf/bundles/shell.yaml"
  --print (b :: Bundle)
