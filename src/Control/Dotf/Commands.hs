module Control.Dotf.Commands (
  module Control.Dotf.Commands.Bundles,
  module Control.Dotf.Commands.Os,
  listTracked,
  listTrackedAll,
  listUntracked,
  unsafeListTracked,
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
  diffState,
) where

import Control.Dotf.Commands.Bundles
import Control.Dotf.Commands.Git
import Control.Dotf.Commands.Os
import Data.Dotf
import Data.Either (fromRight)
import System.Directory (getHomeDirectory)
import System.Process.Typed (ExitCode)

listTrackedAll :: IO ErrorOrTracked
listTrackedAll = do
  tracked <- fmap (map Tracked) . processFileListResult <$> gitTracked
  rstaged <- processFileListResult <$> gitTrackedStaged
  rustaged <- processFileListResult <$> gitTrackedUnstaged
  staged <- mapEitherM Staged rstaged
  unstaged <- mapEitherM Unstaged rustaged
  return $ comb <$> tracked <*> staged <*> unstaged
 where
  comb a b c = a ++ b ++ c

listTracked :: IO ErrorOrTracked
listTracked = do
  rstaged <- processFileListResult <$> gitTrackedStaged
  runstaged <- processFileListResult <$> gitTrackedUnstaged
  staged <- mapEitherM Staged rstaged
  unstaged <- mapEitherM Unstaged runstaged
  return $ comb <$> staged <*> unstaged
 where
  comb a b = a ++ b

unsafeListTracked :: Bool -> IO [TrackedType]
unsafeListTracked True = fmap (fromRight []) listTrackedAll
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
