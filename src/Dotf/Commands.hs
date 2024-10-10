module Dotf.Commands (
  checkRequirements,
  installRequirements,
  installBundles,
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
  clone,
  newBareRepo,
  push,
  pull,
  status,
  diffState
) where

import           Control.Monad        (forM_, void, (>=>))
import           Control.Monad.Extra  (whenM)
import           Data.Either          (fromRight)
import           Dotf.Bundles         (Cloneable (toCloneProcess),
                                       Installable (toInstallProcess),
                                       collectGitPackages, collectPackages,
                                       collectPostScripts, collectPreScripts,
                                       loadBundles)
import           Dotf.Git             (gitCheckoutBare, gitCloneBareUrl,
                                       gitCommit, gitDiffFile, gitDiffStatus,
                                       gitIgnoreFile, gitNewBareRepo, gitPull,
                                       gitPush, gitStageFile, gitStatus,
                                       gitTracked, gitTrackedStaged,
                                       gitTrackedUnstaged, gitUnstageFile,
                                       gitUntrackFile, gitUntracked, mapEitherM,
                                       processFileListResult,
                                       processStringResult)
import           Dotf.Types           (Distro (Arch, Osx, Unsupported), Dry,
                                       ErrorOrFilePaths, ErrorOrString,
                                       ErrorOrTracked, GitPackage, Package,
                                       TrackedType (..))
import           Dotf.Utils           (appendToFile, distro, resolveScript,
                                       which)
import           System.Directory     (createDirectoryIfMissing,
                                       doesDirectoryExist, getHomeDirectory)
import           System.FilePath      ((</>))
import qualified System.Process.Typed as PT
import           System.Process.Typed (ExitCode)

checkRequirements :: IO Bool
checkRequirements = distro >>= osWhich
  where osWhich Arch = which "paru"
        osWhich Osx  = which "brew"
        osWhich _    = pure True

installRequirements :: Dry -> IO ()
installRequirements d = distro >>= osInstall
  where osInstall Arch = installParu d
        osInstall Osx  = installHomebrew d
        osInstall _    = pure ()

installBundles :: Dry -> IO ()
installBundles dry = do
  dist    <- distro
  bundles <- loadBundles
  let pkgs    = collectPackages bundles
      gits    = collectGitPackages bundles
      pre     = collectPreScripts bundles
      post    = collectPostScripts bundles
      preIO   = installPre dry pre
      pkgIO   = installPackages dry dist pkgs
      cloneIO = cloneGitPackages dry gits
      gitIO   = installGitPackages dry gits
      postIO  = installPost dry post

  preIO >> pkgIO >> cloneIO >> gitIO >> postIO

listTrackedAll :: IO ErrorOrTracked
listTrackedAll = do
  tracked  <- fmap (map Tracked) . processFileListResult <$> gitTracked
  rstaged  <- processFileListResult <$> gitTrackedStaged
  rustaged <- processFileListResult <$> gitTrackedUnstaged
  staged   <- mapEitherM Staged rstaged
  unstaged <- mapEitherM Unstaged rustaged
  return $ comb <$> tracked <*> staged <*> unstaged
  where comb a b c = a ++ b ++ c

listTracked :: IO ErrorOrTracked
listTracked = do
  rstaged   <- processFileListResult <$> gitTrackedStaged
  runstaged <- processFileListResult <$> gitTrackedUnstaged
  staged    <- mapEitherM Staged rstaged
  unstaged  <- mapEitherM Unstaged runstaged
  return $ comb <$> staged <*> unstaged
  where comb a b = a ++ b

unsafeListTracked :: Bool -> IO [TrackedType]
unsafeListTracked True  = fmap (fromRight []) listTrackedAll
unsafeListTracked False = fmap (fromRight []) listTracked

listUntracked :: IO ErrorOrFilePaths
listUntracked = processFileListResult <$> gitUntracked

unsafeListUntracked :: IO [FilePath]
unsafeListUntracked = fmap (fromRight []) listUntracked

ignoreFile :: FilePath -> IO ()
ignoreFile fp = getHomeDirectory >>= (appendToFile fp . gitIgnoreFile)

stageFile :: FilePath -> IO ExitCode
stageFile = gitStageFile

unstageFile :: FilePath -> IO ExitCode
unstageFile = gitUnstageFile

untrackFile :: FilePath -> IO ExitCode
untrackFile = gitUntrackFile

diffFile :: FilePath -> IO ErrorOrString
diffFile fp = processStringResult <$> gitDiffFile fp

commit :: Dry -> String -> IO ()
commit = gitCommit

clone :: Dry -> String -> IO ()
clone d url = gitCloneBareUrl d url >> gitCheckoutBare d

newBareRepo :: Dry -> IO ()
newBareRepo d = do
  home <- getHomeDirectory
  gitNewBareRepo d (home </> ".dotf")

push :: Dry -> IO ()
push = gitPush

pull :: Dry -> IO ()
pull = gitPull

status :: Dry -> IO ()
status = gitStatus

diffState :: Dry -> IO ()
diffState = gitDiffStatus

-----------
-- Utils --
-----------

installParu :: Dry -> IO ()
installParu dry = do
  home <- getHomeDirectory
  let dir  = home </> ".local" </> "share" </> "paru"
      clne = PT.proc "git" ["clone", "https://aur.archlinux.org/paru.git", dir]
      inst = PT.setWorkingDir dir $ PT.proc "makepkg" ["-si", "--noconfirm"]
  whenM (doesDirectoryExist dir) (createDirectoryIfMissing True dir)
  if dry
    then print clne >> print inst
    else void $ PT.runProcess clne >> PT.runProcess inst

installHomebrew :: Dry -> IO ()
installHomebrew dry = do
  let curl = "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
      proc = PT.proc "bash" ["-c", curl]
  if dry
    then print proc
    else void $ PT.runProcess proc

installPackages :: Dry -> Distro -> [Package] -> IO ()
installPackages True d pkgs = toInstallProcess d pkgs >>= print
installPackages _ d pkgs    = toInstallProcess d pkgs >>= PT.runProcess >> pure ()

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
