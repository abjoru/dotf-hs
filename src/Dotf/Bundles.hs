{-# LANGUAGE FlexibleInstances #-}
module Dotf.Bundles (
  Cloneable(..),
  Installable(..),

  loadBundles,
  newBundle,
  listNewPackages,
  collectPackages,
  collectNamedPackages,
  collectGitPackages,
  collectScripts
) where

import           Data.String.Interpolate (i)
import qualified Data.Yaml               as Y
import           Dotf.Templates          (bundleFileTemplate)
import           Dotf.Types
import           Dotf.Utils
import           System.Directory        (XdgDirectory (XdgCache, XdgConfig),
                                          createDirectoryIfMissing,
                                          doesDirectoryExist, getXdgDirectory)
import           System.FilePath         ((</>))
import           System.Process.Typed    (ProcessConfig, proc)

-----------------
-- Typeclasses --
-----------------

class Cloneable a where
  toCloneProcess :: a -> IO (ProcessConfig () () ())

class Installable a where
  toInstallProcess :: Distro -> a -> IO (ProcessConfig () () ())

---------------
-- Instances --
---------------

instance Installable [Package] where
  toInstallProcess Arch px = (\x -> proc "paru" $ "-S" : x) <$> listNewPackages px
  toInstallProcess Osx px  = (\x -> proc "brew" $ "install" : x) <$> listNewPackages px
  toInstallProcess Deb px  = (\x -> proc "apt-get" $ "install" : x) <$> listNewPackages px
  toInstallProcess _ _     = return $ proc "echo" ["Unsupported distro!"]

instance Cloneable GitPackage where
  toCloneProcess pkg = do
    dir <- (</> gitName pkg) <$> getXdgDirectory XdgCache "dotf"
    upd <- doesDirectoryExist dir
    return $ proc "git" (args dir upd pkg)
    where args _ True (GitPackage _ _ _ True _)      = ["pull", "--recurse-submodules"]
          args _ True _                              = ["pull"]
          args d _ (GitPackage _ u Nothing False _)  = ["clone", u, d]
          args d _ (GitPackage _ u Nothing True _)   = ["clone", "--recurse-submodules", u, d]
          args d _ (GitPackage _ u (Just b) False _) = ["clone", "-b", b, u, d]
          args d _ (GitPackage _ u (Just b) True _)  = ["clone", "--recurse-submodules", "-b", b, u, d]

instance Installable GitPackage where
  toInstallProcess _ (GitPackage n _ _ _ (Just cmd)) = do
    dir <- (</> n) <$> getXdgDirectory XdgCache "dotf"
    return $ proc "bash" ["-c", pushPop dir cmd]
  toInstallProcess _ p = return $ proc "echo" [[i|No install command for GIT package '#{gitName p}'!|]]

-------------
-- Methods --
-------------

loadBundles :: IO [Bundle]
loadBundles = listBundleFiles >>= mapM Y.decodeFileThrow

newBundle :: String -> IO ()
newBundle name = do
  base <- getXdgDirectory XdgConfig "dotf"

  let configDir = base </> "bundles"
      configFile = configDir </> (name ++ ".yaml")

  createDirectoryIfMissing True configDir
  writeFile configFile $ bundleFileTemplate name

listNewPackages :: [Package] -> IO [String]
listNewPackages allPkgs = do
  dist <- distro
  installed <- listInstalledPackages dist
  let pkgs = filterOsPackages dist allPkgs
      res  = filter (`notElem` installed) pkgs
  return res

collectPackages :: [Bundle] -> [Package]
collectPackages = foldr (\b acc -> packages b ++ acc) []

collectNamedPackages :: [Bundle] -> [NamedPackage]
collectNamedPackages = foldr (\b acc -> bundleOsPackages b ++ acc) []

collectGitPackages :: [Bundle] -> [GitPackage]
collectGitPackages = foldr (\b acc -> bundleGitPackages b ++ acc) []

collectScripts :: [Bundle] -> [FilePath]
collectScripts = foldr (\b acc -> scripts b ++ acc) []

-----------
-- Utils --
-----------

scripts :: Bundle -> [FilePath]
scripts v = mkScriptList (bundlePreInstallScript v) (bundlePostInstallScript v)
  where mkScriptList (Just a) (Just b) = [a, b]
        mkScriptList (Just a) Nothing  = [a]
        mkScriptList Nothing (Just b)  = [b]
        mkScriptList _ _               = []

packages :: Bundle -> [Package]
packages b = map (\(NamedPackage _ p) -> p) $ bundleOsPackages b

filterOsPackages :: Distro -> [Package] -> [String]
filterOsPackages dist = foldr (find dist) []
  where find Arch (Package (Just p) _ _) xs = p : xs
        find Osx (Package _ (Just p) _) xs  = p : xs
        find Deb (Package _ _ (Just p)) xs  = p : xs
        find _ _ xs                         = xs
