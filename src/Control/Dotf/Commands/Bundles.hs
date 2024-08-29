module Control.Dotf.Commands.Bundles (
  listBundles,
  newBundle,
) where

import Data.Dotf
import qualified Data.Yaml as Y
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))

listBundles :: IO [Bundle]
listBundles = listBundleFiles >>= mapM Y.decodeFileThrow

newBundle :: String -> IO ()
newBundle name = do
  base <- getXdgDirectory XdgConfig "dotf"

  let configDir = base </> "bundles"
      configFile = configDir </> (name ++ ".yaml")

  createDirectoryIfMissing True configDir
  writeFile configFile $ bundleFileTemplate name
