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
import           Data.Either          (fromRight)
import           Dotf.Bundles
import           Dotf.Git
import           Dotf.Types
import           Dotf.Utils           (appendToFile, distro, pushPop, which)
import           System.Directory     (getHomeDirectory)
import qualified System.Process.Typed as PT
import           System.Process.Typed (ExitCode)

checkRequirements :: Dry -> IO Bool
checkRequirements True = distro >>= osWhich
  where osWhich Arch = putStrLn "which paru" >> pure True
        osWhich Osx  = putStrLn "which brew" >> pure True
        osWhich _    = pure True
checkRequirements False = distro >>= osWhich
  where osWhich Arch = which "paru"
        osWhich Osx  = which "brew"
        osWhich _    = pure True

installRequirements :: Dry -> IO ()
installRequirements d = distro >>= osInstall
  where osInstall Arch = installParu d
        osInstall Osx  = installHomebrew d
        osInstall _    = pure ()

installBundles :: Dry -> [Bundle] -> IO ()
installBundles dry bundles = do
  dist <- distro
  let pkgs = collectPackages bundles
      gits = collectGitPackages bundles
  installPackages dry dist pkgs >> cloneGitPackages dry gits >> installGitPackages dry gits

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

commit :: String -> IO ExitCode
commit = gitCommit

clone :: String -> IO ExitCode
clone = gitCloneBareUrl

newBareRepo :: FilePath -> IO ExitCode
newBareRepo = gitNewBareRepo

push :: IO ExitCode
push = gitPush

pull :: IO ExitCode
pull = gitPull

status :: IO ExitCode
status = gitStatus

diffState :: IO ExitCode
diffState = gitDiffStatus

-----------
-- Utils --
-----------

installParu :: Dry -> IO ()
installParu dry = do
  let dir  = "~/.local/share/paru"
      clne = PT.proc "git" ["https://aur.archlinux.org/paru.git", dir]
      inst = PT.proc "bash" ["-C", pushPop dir "makepkg -si --noconfirm"]
  if dry
    then print clne >> print inst
    else void $ PT.runProcess clne >> PT.runProcess inst

installHomebrew :: Dry -> IO ()
installHomebrew _ = fail "Not implemented!"

installPackages :: Dry -> Distro -> [Package] -> IO ()
installPackages True d pkgs = toInstallProcess d pkgs >>= print
installPackages _ d pkgs    = toInstallProcess d pkgs >>= PT.runProcess >> pure ()

cloneGitPackages :: Dry -> [GitPackage] -> IO ()
cloneGitPackages True pkgs = forM_ pkgs (toCloneProcess >=> print)
cloneGitPackages _ pkgs    = forM_ pkgs (toCloneProcess >=> PT.runProcess)

installGitPackages :: Dry -> [GitPackage] -> IO ()
installGitPackages True pkgs = forM_ pkgs (toInstallProcess Unsupported >=> print)
installGitPackages _ pkgs    = forM_ pkgs (toInstallProcess Unsupported >=> PT.runProcess)
