module Dotf.Commands (
  checkRequirements,
  installRequirements,
  installBundles,
  updateBundles,
  listTrackedAll,
  listTracked,
  unsafeListTracked,
  listUntracked,
  unsafeListUntracked,
  ignoreFile,
  stageFile,
  unstageFile,
  untrackFile,
  diffFile,
  commit,
  commit',
  clone,
  newBareRepo,
  push,
  pull,
  status,
  diffState,
  gitRaw
) where

import           Control.Monad        (forM_, void, (>=>))
import           Control.Monad.Extra  (whenM)
import           Data.Either          (fromRight)
import           Dotf.Bundles         (Cloneable (toCloneProcess),
                                       Installable (toInstallProcess),
                                       collectGitPackages, collectPackages,
                                       collectPostScripts, collectPreScripts,
                                       loadBundles)
import           Dotf.Git             (gitBare, gitBareSilent, gitTracked,
                                       gitTrackedStaged, gitTrackedUnstaged,
                                       gitUntracked, mapEitherM,
                                       processFileListResult,
                                       processStringResult)
import           Dotf.Types           (Bundle (..),
                                       Distro (Arch, Osx, Unsupported), Dry,
                                       ErrorOrFilePaths, ErrorOrString,
                                       ErrorOrTracked, GitPackage, Headless,
                                       Package, TrackedType (..))
import           Dotf.Utils           (appendToFile, ask', distro,
                                       gitIgnoreFile, resolveScript, which)
import           System.Directory     (createDirectoryIfMissing,
                                       doesDirectoryExist, doesFileExist,
                                       getHomeDirectory)
import           System.FilePath      ((</>))
import qualified System.Process.Typed as PT
import           System.Process.Typed (ExitCode)

-- | Check that requirements for the underlying OS
-- is satisfied. The base checks are basically for
-- package managers that 'dotf' relies on.
checkRequirements :: IO Bool
checkRequirements = distro >>= osWhich
  where osWhich Arch = which "paru"
        osWhich Osx  = which "brew"
        osWhich _    = pure True

-- | Install required dependencies for the underlying
-- OS. In the case of 'Arch Linux', this would be the
-- package manager 'paru', and on 'OSX' it would be
-- the package manager 'homebrew'.
installRequirements :: Dry -> IO ()
installRequirements d = distro >>= osInstall
  where osInstall Arch = installParu d
        osInstall Osx  = installHomebrew d
        osInstall _    = pure ()

-- | Install all application bundles for the underlying
-- OS. This function will attempt to filter out packages
-- that have already been installed, so it is safe to
-- call after initial setup. Furthermore, any GIT
-- package will turn from 'clone/install' to
-- 'pull/update' if it already exist. Any pre/post bash
-- scripts will be run accordingly.
installBundles :: Dry -> Headless -> IO ()
installBundles dry h = do
  dist    <- distro
  bundles <- loadBundles
  let filtered = filter (matchHeadless h . bundleHeadless) $ fromRight [] bundles
      pkgs     = collectPackages filtered
      gits     = collectGitPackages dist filtered
      pre      = collectPreScripts filtered
      post     = collectPostScripts filtered
      preIO    = installPre dry pre
      pkgIO    = installPackages dry dist pkgs
      cloneIO  = cloneGitPackages dry gits
      gitIO    = installGitPackages dry gits
      postIO   = installPost dry post

  preIO >> pkgIO >> cloneIO >> gitIO >> postIO

updateBundles :: Dry -> Headless -> IO ()
updateBundles dry h = do
  dist <- distro
  bundles <- loadBundles
  let filtered = filter (matchHeadless h . bundleHeadless) $ fromRight [] bundles
      pkgs     = collectPackages filtered
      gits     = collectGitPackages dist filtered
      pkgIO    = whenM askUpdPkg $ installPackages dry dist pkgs
      gitIO    = whenM askUpdGit (cloneGitPackages dry gits >> installGitPackages dry gits)

  pkgIO >> gitIO
  where askUpdPkg = ask' "Do you want to update packages? (y/N) "
        askUpdGit = ask' "Do you want to update GIT packages? (y/N) "

-- system headless -> bundle headless -> Bool
-- Should return 'False' if system is run in headless mode
-- but bundle does not have headless set to 'True'.
matchHeadless :: Headless -> Headless -> Bool
matchHeadless True False = False
matchHeadless _ _        = True

-- | List ALL tracked files. This includes unchanged
-- tracked files, staged changes, as well as unstaged
-- changes.
listTrackedAll :: IO ErrorOrTracked
listTrackedAll = do
  tracked  <- fmap (map Tracked) . processFileListResult <$> gitTracked
  rstaged  <- processFileListResult <$> gitTrackedStaged
  rustaged <- processFileListResult <$> gitTrackedUnstaged
  staged   <- mapEitherM Staged rstaged
  unstaged <- mapEitherM Unstaged rustaged
  return $ comb <$> tracked <*> staged <*> unstaged
  where comb a b c = a ++ b ++ c

-- | List staged and unstaged changes only.
listTracked :: IO ErrorOrTracked
listTracked = do
  rstaged   <- processFileListResult <$> gitTrackedStaged
  runstaged <- processFileListResult <$> gitTrackedUnstaged
  staged    <- mapEitherM Staged rstaged
  unstaged  <- mapEitherM Unstaged runstaged
  return $ comb <$> staged <*> unstaged
  where comb a b = a ++ b

-- | Unsafe version of `listTracked` that defaults
-- to empty list on error. It is unsafe in that errors
-- that may occur are swallowed in favor of empty default.
unsafeListTracked :: Bool -> IO [TrackedType]
unsafeListTracked True  = fmap (fromRight []) listTrackedAll
unsafeListTracked False = fmap (fromRight []) listTracked

-- | List all untracked files.
listUntracked :: IO ErrorOrFilePaths
listUntracked = processFileListResult <$> gitUntracked

-- | Unsafe version of `listUntracked` that defaults
-- to empty list on error. It is unsafe in that errors
-- that may occur are swallowed in favor of empty default.
unsafeListUntracked :: IO [FilePath]
unsafeListUntracked = fmap (fromRight []) listUntracked

-- | Appends the given file to the GIT ignore file.
ignoreFile :: FilePath -> IO ()
ignoreFile fp = getHomeDirectory >>= (appendToFile fp . gitIgnoreFile)

-- | Stage changes for the given file.
stageFile :: FilePath -> IO ExitCode
stageFile fp = gitBare ["add", fp] >>= PT.runProcess

-- | Unstage changes for the given file.
unstageFile :: FilePath -> IO ExitCode
unstageFile fp = gitBareSilent ["reset", "--", fp] >>= PT.runProcess

-- | Untrack the given file removing it from the GIT index.
untrackFile :: FilePath -> IO ExitCode
untrackFile fp = do
  home <- getHomeDirectory
  exists <- doesFileExist $ home </> fp
  if exists
    then gitBare ["rm", "--cached", fp] >>= PT.runProcess
    else gitBareSilent ["rm", fp] >>= PT.runProcess

-- | Diff the given file.
diffFile :: FilePath -> IO ErrorOrString
diffFile fp = do
  res <- gitBare ["diff", fp] >>= PT.readProcess
  return $ processStringResult res

-- | Commit all staged changes with the given message.
-- This function can be called in 'Dry' mode which would
-- print the commands instead of running them.
commit :: Dry -> String -> IO ()
commit True msg  = gitBare ["commit", "-m", msg] >>= print
commit False msg = gitBare ["commit", "-m", msg] >>= (void . PT.runProcess)

-- | Alternative to `commit` that does not support `Dry`
-- and redirects all output to '/dev/null'. To be used in
-- TUI.
commit' :: String -> IO ()
commit' msg = gitBareSilent ["commit", "-m", msg] >>= (void . PT.runProcess)

-- | Clone a given repo as a 'bare' GIT repository into
-- the users `HOME` directory. The GIT config directory
-- will be set to `~/.dotf` and a `checkout` will be
-- issued after the clone.
clone :: Dry -> String -> IO ()
clone d url = do
  home <- getHomeDirectory
  cfg1 <- gitBare ["clone", "--bare", url, home </> ".dotf"]
  cfg2 <- gitBare ["checkout"]
  runCmd d cfg1 cfg2
  where runCmd True a b  = print a >> print b
        runCmd False a b = void $ PT.runProcess a >> PT.runProcess b

-- | Initialize a new 'bare' GIT repository in the users
-- `HOME` directory. The GIT config directory will be set
-- to `~/.dotf`.
newBareRepo :: Dry -> IO ()
newBareRepo dry = getHomeDirectory >>= mkCmd >>= if dry then print else void . PT.runProcess
  where mkCmd h = gitBare ["init", "--bare", h </> ".dotf"]

-- | Pushes all committed changes upstream.
-- This function can also be run in `Dry` mode where the
-- process configuration will be printed to the terminal
-- instead of being executed.
push :: Dry -> IO ()
push False = gitBare ["push"] >>= (void . PT.runProcess)
push True  = gitBare ["push"] >>= print

-- | Pulls all remote changes.
-- This function can also be run in `Dry` mode where the
-- process configuration will be printed to the terminal
-- instead of being executed.
pull :: Dry -> IO ()
pull False = gitBare ["pull"] >>= (void . PT.runProcess)
pull True  = gitBare ["pull"] >>= print

-- | Executes GIT 'status'.
-- This function can also be run in `Dry` mode where the
-- process configuration will be printed to the terminal
-- instead of being executed.
status :: Dry -> IO ()
status False = gitBare ["status", "-sb"] >>= (void . PT.runProcess)
status True  = gitBare ["status", "-sb"] >>= print

-- | Executes GIT 'diff'.
-- This function can also be run in `Dry` mode where the
-- process configuration will be printed to the terminal
-- instead of being executed.
diffState :: Dry -> IO ()
diffState False = gitBare ["diff"] >>= (void . PT.runProcess)
diffState True  = gitBare ["diff"] >>= print

-- | Executes a 'raw' GIT command. This function accepts
-- the arguments to the `git` command and executes it.
-- This function can also be run in `Dry` mode where the
-- process configuration will be printed to the terminal
-- instead of being executed.
--
-- ==== __Examples__
--
-- >>> gitRaw False "status -sb"
gitRaw :: Dry -> String -> IO ()
gitRaw False cmd = gitBare (words cmd) >>= (void . PT.runProcess)
gitRaw True cmd  = gitBare (words cmd) >>= print

-----------
-- Utils --
-----------

paruUrl :: String
paruUrl = "https://aur.archlinux.org/paru.git"

homebrewUrl :: String
homebrewUrl = "https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"

installParu :: Dry -> IO ()
installParu dry = do
  home <- getHomeDirectory
  let dir  = home </> ".local" </> "share" </> "paru"
      clne = PT.proc "git" ["clone", paruUrl, dir]
      inst = PT.setWorkingDir dir $ PT.proc "makepkg" ["-si", "--noconfirm"]
  whenM (doesDirectoryExist dir) (createDirectoryIfMissing True dir)
  if dry
    then print clne >> print inst
    else void $ PT.runProcess clne >> PT.runProcess inst

installHomebrew :: Dry -> IO ()
installHomebrew dry = do
  let curl = "$(curl -fsSL " ++ homebrewUrl ++ ")"
      proc = PT.proc "bash" ["-c", curl]
  if dry
    then print proc
    else void $ PT.runProcess proc

installPackages :: Dry -> Distro -> [Package] -> IO ()
installPackages True d pkgs = toInstallProcess d pkgs >>= print
installPackages _ d pkgs    = toInstallProcess d pkgs >>= PT.runProcess >> pure ()

-- updatePackages :: Dry -> Distro -> IO ()
-- updatePackages dry Arch = do
  -- let p = PT.proc "paru" ["-Syyu"]
  -- if dry
    -- then print p
    -- else void $ PT.runProcess p
-- updatePackages dry Osx = do
  -- let p1 = PT.proc "brew" ["update"]
      -- p2 = PT.proc "brew" ["upgrade"]
  -- if dry
    -- then print p1 >> print p2
    -- else void (PT.runProcess p1 >> PT.runProcess p2)
-- updatePackages _ _ = putStrLn "Unable to update packages!"

cloneGitPackages :: Dry -> [GitPackage] -> IO ()
cloneGitPackages True pkgs = forM_ pkgs (toCloneProcess >=> print)
cloneGitPackages _ pkgs    = forM_ pkgs (toCloneProcess >=> PT.runProcess)

installGitPackages :: Dry -> [GitPackage] -> IO ()
installGitPackages True pkgs = forM_ pkgs (toInstallProcess Unsupported >=> print)
installGitPackages _ pkgs    = forM_ pkgs (toInstallProcess Unsupported >=> PT.runProcess)

installPre :: Dry -> [FilePath] -> IO ()
installPre True fp  = forM_ fp (mkScriptProc >=> print)
installPre False fp = forM_ fp (mkScriptProc >=> PT.runProcess)

installPost :: Dry -> [FilePath] -> IO ()
installPost True fp  = forM_ fp (mkScriptProc >=> print)
installPost False fp = forM_ fp (mkScriptProc >=> PT.runProcess)

mkScriptProc :: FilePath -> IO (PT.ProcessConfig () () ())
mkScriptProc f = (\v -> PT.proc "bash" ["-c", v]) <$> resolveScript f
