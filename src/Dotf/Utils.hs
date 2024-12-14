module Dotf.Utils (
  editFile,
  gitIgnoreFile,
  gitDir,
  workTree,
  runDiff,
  which,
  distro,
  appendToFile,
  resolveAppConfig,
  listBundleFiles,
  listInstalledPackages,
  resolveBundleFile,
  resolveScript,
  resolveScript',
  resolveDotFile,
  readOverrides,
  getGitInstallPath,
  ask,
  ask'
) where

import           Control.Exception       (SomeException, try)
import           Control.Monad           (filterM)
import           Control.Monad.Extra     (ifM)
import qualified Data.ConfigFile         as CF
import           Data.Either             (fromRight)
import           Data.Either.Utils       (forceEither)
import qualified Data.Map                as M
import           Data.String.Interpolate (i)
import           Dotf.Types              (Answer (..),
                                          Distro (Arch, Deb, Osx, Unsupported),
                                          GitPackage (gitInstallPath), gitName)
import           System.Directory        (XdgDirectory (XdgCache, XdgConfig),
                                          doesDirectoryExist, doesFileExist,
                                          getHomeDirectory, getXdgDirectory,
                                          listDirectory)
import           System.FilePath         (takeExtension, (</>))
import           System.Info             (os)
import           System.IO               (IOMode (WriteMode), hClose, hFlush,
                                          stdout, withFile)
import           System.OsRelease        (OsRelease (name),
                                          OsReleaseResult (osRelease),
                                          parseOsRelease)
import           System.Process          (CreateProcess (cwd, std_in, std_out),
                                          StdStream (CreatePipe, UseHandle),
                                          callProcess, createProcess, proc,
                                          readProcess, waitForProcess)

-- | Open 'nvim' with the given file as input.
editFile :: FilePath -> IO ()
editFile file = callProcess "nvim" [file]

-- | Resolve the GIT ignore file.
gitIgnoreFile :: FilePath -> FilePath
gitIgnoreFile base = base </> ".gitignore"

-- | Pure GIT dir argument.
gitDir :: String -> String
gitDir d = [i|--git-dir=#{d </> ".dotf"}|]

-- | Pure GIT working tree argument.
workTree :: String -> String
workTree d = [i|--work-tree=#{d}|]

-- | Run GIT diff on a single file or entire index.
-- This function will open the diff in less.
runDiff :: Maybe FilePath -> IO ()
runDiff mf = do
  home <- getHomeDirectory
  let args = args' home mf
  (_, Just out, _, ph1) <- createProcess (proc "git" args) { std_out = CreatePipe, cwd = Just home }
  (_, _, _, ph2)        <- createProcess (proc "less" ["-R"]) { std_in = UseHandle out }

  hClose out
  _ <- waitForProcess ph1
  _ <- waitForProcess ph2
  return ()
  where args' h Nothing  = [gitDir h, workTree h, "diff", "--color"]
        args' h (Just f) = [gitDir h, workTree h, "diff", "--color", h </> f]

-- | Run 'which' to determine if the program exist.
which :: String -> IO Bool
which cmd = do
  rs <- try (readProcess "which" [cmd] "") :: IO (Either SomeException String)
  case rs of
    Left _  -> return False
    Right o -> return $ not $ null o

-- | Find the underlying distribution if supported.
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

-- | Append a line to a given file.
appendToFile :: String -> FilePath-> IO ()
appendToFile line file = do
  exists <- doesFileExist file
  if exists
    then appendFile file (line ++ "\n")
    else do
      withFile file WriteMode (\_ -> return ())
      appendFile file (line ++ "\n")

resolveAppConfig :: IO FilePath
resolveAppConfig = (</> "applications.yaml") <$> getXdgDirectory XdgConfig "xmonad"

-- | List all bundle files.
listBundleFiles :: IO [FilePath]
listBundleFiles = do
  cfgDir <- getXdgDirectory XdgConfig "dotf"
  listFilesInDir (cfgDir </> "bundles") yamlFilter
  where yamlFilter fp =
          let ext = takeExtension fp
           in ext == ".yaml" || ext == ".yml"

-- | List all installed packages.
-- This function uses 'pacman', 'homebrew', or
-- 'dpkg-query' based on underlying OS.
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

-- | List all files in a given directory that matches
-- provided filter.
listFilesInDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
listFilesInDir dir f = ifM (doesDirectoryExist dir) lsf (pure [])
  where lsf = do
          relPath <- listDirectory dir
          let absPath = map (dir </>) relPath
          files <- filterM doesFileExist absPath
          return $ filter f files

-- | Resolve absolute path for a bundle file.
resolveBundleFile :: Maybe String -> IO (Maybe FilePath)
resolveBundleFile (Just relName) = do
  baseDir <- getXdgDirectory XdgConfig "dotf"
  return $ Just (baseDir </> "bundles" </> relName)
resolveBundleFile Nothing = pure Nothing

-- | Resolve absolute path of script file.
resolveScript :: String -> IO FilePath
resolveScript relName = (</> relName) <$> getXdgDirectory XdgConfig "dotf"

-- | Alternative to `resolveScript` operating on `Maybe`.
resolveScript' :: Maybe String -> IO (Maybe FilePath)
resolveScript' (Just relName) = Just <$> resolveScript relName
resolveScript' Nothing        = pure Nothing

-- | Resolve absolute path of dot-file.
resolveDotFile :: Maybe FilePath -> IO (Maybe FilePath)
resolveDotFile m = (\home -> (home </>) <$> m) <$> getHomeDirectory

-- | Read GIT install path override file and turn it into
-- a map from name -> file path (with base of $HOME)
readOverrides :: IO (M.Map String String)
readOverrides = do
  base <- getXdgDirectory XdgConfig "dotf"
  let file = base </> "overrides.cfg"
  ifM (doesFileExist file) (readMap file) (pure M.empty)
  where readMap f = do
          cfg <- CF.readfile CF.emptyCP f
          return $ toMap $ fromRight CF.emptyCP cfg
        toMap cp = M.fromList $ forceEither $ CF.items cp "DEFAULT"

-- | Get the GIT install path for a given package. This
-- function will return default path unless the override
-- map contains an entry for the GIT package name. The
-- paths returned are always absolute.
getGitInstallPath :: M.Map String String -> GitPackage -> IO FilePath
getGitInstallPath m pkg = do
  home  <- getHomeDirectory
  cache <- getXdgDirectory XdgCache "dotf"
  return $ case M.lookup (gitName pkg) m of
    Just p  -> home </> p
    Nothing -> resolve home cache (gitInstallPath pkg) (gitName pkg)
  where resolve home _ (Just rel) _ = home </> rel
        resolve _ cache _ n         = cache </> n

ask :: String -> IO Answer
ask msg = putStrLn msg >> hFlush stdout >> read <$> getLine

ask' :: String -> IO Bool
ask' msg = checkAnswer <$> ask msg
  where checkAnswer Yes = True
        checkAnswer _   = False
