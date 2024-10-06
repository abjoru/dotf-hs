module Dotf.Utils (
  editFile,
  which,
  pushPop,
  distro,
  appendToFile,
  listBundleFiles,
  listInstalledPackages,
  resolveBundleFile,
  resolveScriptFile,
  resolveGitFile
) where

import           Control.Exception       (SomeException, try)
import           Control.Monad           (filterM)
import           Data.String.Interpolate (i)
import           Dotf.Types              (Distro (Arch, Deb, Osx, Unsupported))
import           System.Directory        (XdgDirectory (XdgConfig),
                                          doesDirectoryExist, doesFileExist,
                                          getHomeDirectory, getXdgDirectory,
                                          listDirectory)
import           System.FilePath         (takeExtension, (</>))
import           System.Info             (os)
import           System.IO               (IOMode (WriteMode), withFile)
import           System.OsRelease        (OsRelease (name),
                                          OsReleaseResult (osRelease),
                                          parseOsRelease)
import           System.Process          (callProcess, readProcess)

editFile :: FilePath -> IO ()
editFile file = callProcess "nvim" [file]

which :: String -> IO Bool
which cmd = do
  rs <- try (readProcess "which" [cmd] "") :: IO (Either SomeException String)
  case rs of
    Left _  -> return False
    Right o -> return $ not $ null o

pushPop :: String -> String -> String
pushPop dir cmd =
  let push = ["pushd", dir, ">", "/dev/null", "2>&1"]
      pop  = ["popd", ">", "/dev/null", "2>&1"]
   in [i|#{unwords push} ; #{cmd} ; #{unwords pop}|]

distro :: IO Distro
distro = case os of
  "linux"  -> maybe Unsupported (findName . osRelease) <$> parseOsRelease
  "darwin" -> pure Osx
  _        -> pure Unsupported
  where findName o = case name o of
          "Arch Linux" -> Arch
          "Ubuntu"     -> Deb
          "Debian"     -> Deb
          _            -> Unsupported

appendToFile :: String -> FilePath-> IO ()
appendToFile line file = do
  exists <- doesFileExist file
  if exists
    then appendFile file (line ++ "\n")
    else do
      withFile file WriteMode (\_ -> return ())
      appendFile file (line ++ "\n")

listBundleFiles :: IO [FilePath]
listBundleFiles = do
  cfgDir <- getXdgDirectory XdgConfig "dotf"
  listFilesInDir (cfgDir </> "bundles") yamlFilter
  where yamlFilter fp =
          let ext = takeExtension fp
           in ext == ".yaml" || ext == ".yml"

listInstalledPackages :: Distro -> IO [String]
listInstalledPackages Arch = do
  result <- readProcess "pacman" ["-Q"] ""
  return $ map (head . words) (lines result)
listInstalledPackages Osx = do
  result <- readProcess "brew" ["list"] ""
  return $ lines result
listInstalledPackages Deb = do
  result <- readProcess "dpkg-query" ["-f", "${binary:Package}\n", "-W"] ""
  return $ lines result
listInstalledPackages _ = pure []

listFilesInDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
listFilesInDir dir f = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      relativePaths <- listDirectory dir
      let absPaths = map (dir </>) relativePaths
      files <- filterM doesFileExist absPaths
      return $ filter f files

resolveBundleFile :: Maybe String -> IO (Maybe FilePath)
resolveBundleFile (Just relName) = do
  baseDir <- getXdgDirectory XdgConfig "dotf"
  return $ Just (baseDir </> "bundles" </> relName)
resolveBundleFile Nothing = pure Nothing

resolveScriptFile :: Maybe String -> IO (Maybe FilePath)
resolveScriptFile (Just relName) = do
  baseDir <- getXdgDirectory XdgConfig "dotf"
  return $ Just (baseDir </> relName)
resolveScriptFile Nothing = pure Nothing

resolveGitFile :: Maybe FilePath -> IO (Maybe FilePath)
resolveGitFile (Just relPath) = do
  homeDir <- getHomeDirectory
  return $ Just (homeDir </> relPath)
resolveGitFile Nothing = pure Nothing
