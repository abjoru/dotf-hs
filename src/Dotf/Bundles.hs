{-# LANGUAGE FlexibleInstances #-}
module Dotf.Bundles (
  Cloneable(..),
  Installable(..),
  Updatable(..),

  loadBundles,
  loadBundles',
  newBundle,
  listNewPackages,
  collectPackages,
  collectNamedPackages,
  collectGitPackages,
  collectScripts,
  collectPreScripts,
  collectPostScripts
) where

import           Control.Exception       (try)
import           Data.String.Interpolate (i)
import qualified Data.Yaml               as Y
import           Dotf.Templates          (bundleFileTemplate)
import           Dotf.Types              (Bundle (..), Distro (..),
                                          GitPackage (GitPackage, gitName, gitSkip),
                                          NamedPackage (..), Package (Package))
import           Dotf.Utils              (distro, getGitInstallPath,
                                          listBundleFiles,
                                          listInstalledPackages, readOverrides)
import           System.Directory        (XdgDirectory (XdgConfig),
                                          createDirectoryIfMissing,
                                          doesDirectoryExist, getXdgDirectory)
import           System.FilePath         ((</>))
import           System.Process.Typed    (ProcessConfig, proc, readProcess,
                                          runProcess, setWorkingDir)

-----------------
-- Typeclasses --
-----------------

class Cloneable a where
  toCloneProcess :: a -> IO (ProcessConfig () () ())

class Installable a where
  toInstallProcess :: Distro -> a -> IO (ProcessConfig () () ())

class Updatable a where
  hasUpdates :: a -> IO Bool
  toUpdateProcess :: a -> IO (ProcessConfig () () ())

---------------
-- Instances --
---------------

instance Installable [Package] where
  toInstallProcess Arch px = do
    newPkgs <- listNewPackages px
    case newPkgs of
      [] -> return $ proc "echo" ["No packages to install!"]
      ps -> return $ proc "paru" $ "-S" : ps
  toInstallProcess Osx px  = do
    newPkgs <- listNewPackages px
    case newPkgs of
      [] -> return $ proc "echo" ["No packages to install!"]
      ps -> return $ proc "brew" $ "install" : ps
  toInstallProcess Deb px  = do
    newPkgs <- listNewPackages px
    case newPkgs of
      [] -> return $ proc "echo" ["No packages to install!"]
      ps -> return $ proc "apt-get" $ "install" : ps
  toInstallProcess _ _     = return $ proc "echo" ["Unsupported distro!"]

instance Cloneable GitPackage where
  toCloneProcess pkg = do
    cfg <- readOverrides
    dir <- getGitInstallPath cfg pkg
    upd <- doesDirectoryExist (dir </> ".git")
    if upd
      then return $ setWorkingDir dir $ proc "git" (args dir upd pkg)
      else return $ proc "git" (args dir upd pkg)
    where args _ True (GitPackage _ _ _ _ True _ _)      = ["pull", "--recurse-submodules"]
          args _ True _                                  = ["pull"]
          args d _ (GitPackage _ _ u Nothing False _ _)  = ["clone", u, d]
          args d _ (GitPackage _ _ u Nothing True _ _)   = ["clone", "--recurse-submodules", u, d]
          args d _ (GitPackage _ _ u (Just b) False _ _) = ["clone", "-b", b, u, d]
          args d _ (GitPackage _ _ u (Just b) True _ _)  = ["clone", "--recurse-submodules", "-b", b, u, d]

instance Installable GitPackage where
  toInstallProcess _ pkg@(GitPackage _ _ _ _ _ (Just cmd) _) = do
    cfg <- readOverrides
    dir <- getGitInstallPath cfg pkg
    return $ setWorkingDir dir $ proc "bash" ["-c", cmd]
  toInstallProcess _ p = return $ proc "echo" [[i|No install command for GIT package '#{gitName p}'|]]

-- TODO create 'update' command that pulls and reinstall git packages
-- IFF repo is out of sync.
instance Updatable GitPackage where
  hasUpdates pkg = do
    cfg    <- readOverrides
    dir    <- getGitInstallPath cfg pkg
    _      <- runProcess $ setWorkingDir dir $ proc "git" ["fetch"]
    local  <- readProcess $ setWorkingDir dir $ proc "git" ["rev-parse", "@"]
    remote <- readProcess $ setWorkingDir dir $ proc "git" ["rev-parse", "@{u}"]
    return (local /= remote)

  toUpdateProcess pkg = do
    cfg <- readOverrides
    dir <- getGitInstallPath cfg pkg
    return $ setWorkingDir dir $ proc "git" (args pkg)
    where args (GitPackage _ _ _ _ True _ _) = ["pull", "--recurse-submodules"]
          args _                             = ["pull"]

-------------
-- Methods --
-------------

-- | Load application bundles from disk.
loadBundles :: IO (Either Y.ParseException [Bundle])
loadBundles = listBundleFiles >>= loadBundles'

loadBundles' :: [FilePath] -> IO (Either Y.ParseException [Bundle])
loadBundles' paths = try $ mapM Y.decodeFileThrow paths

-- | Create a new application bundle with a given name.
newBundle :: String -> IO ()
newBundle name = do
  base <- getXdgDirectory XdgConfig "dotf"

  let configDir  = base </> "bundles"
      configFile = configDir </> (name ++ ".yaml")

  createDirectoryIfMissing True configDir
  writeFile configFile $ bundleFileTemplate name

-- | List all 'uninstalled' packages for the underlying OS.
listNewPackages :: [Package] -> IO [String]
listNewPackages allPkgs = do
  dist      <- distro
  installed <- listInstalledPackages dist
  let pkgs = filterOsPackages dist allPkgs
      res  = filter (`notElem` installed) pkgs
  return res

-- | Collect all `Package`s from all `Bundle`s.
collectPackages :: [Bundle] -> [Package]
collectPackages = foldr (\b acc -> packages b ++ acc) []

-- | Alternative to `collectPackages` which returns `NamedPackage`s.
collectNamedPackages :: [Bundle] -> [NamedPackage]
collectNamedPackages = foldr (\b acc -> bundleOsPackages b ++ acc) []

-- | Collect all `GitPackage`s from all `Bundle`s.
collectGitPackages :: Distro -> [Bundle] -> [GitPackage]
collectGitPackages d = foldr (\b acc -> collect d b ++ acc) []
  where collect dist bndl = filter (not . ignore dist) $ bundleGitPackages bndl
        ignore Arch pkg = "arch" `elem` gitSkip pkg
        ignore Osx pkg  = "osx" `elem` gitSkip pkg
        ignore Deb pkg  = "deb" `elem` gitSkip pkg
        ignore _ _      = False

-- | Collect all scripts from all `Bundle`s.
collectScripts :: [Bundle] -> [FilePath]
collectScripts = foldr (\b acc -> scripts b ++ acc) []

-- | Collect 'pre' scripts from all `Bundle`s.
collectPreScripts :: [Bundle] -> [FilePath]
collectPreScripts = foldr coll []
  where coll b acc = case bundlePreInstallScript b of
          (Just s) -> s : acc
          Nothing  -> acc

-- | Collect 'post' scripts from all `Bundle`s.
collectPostScripts :: [Bundle] -> [FilePath]
collectPostScripts = foldr coll []
  where coll b acc = case bundlePostInstallScript b of
          (Just s) -> s : acc
          Nothing  -> acc

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
  where find Arch (Package (Just p) _ _ _) xs = p : xs
        find Osx (Package _ (Just p) _ _) xs  = p : xs
        find Cask (Package _ _ (Just p) _) xs = p : xs
        find Deb (Package _ _ _ (Just p)) xs  = p : xs
        find _ _ xs                           = xs
