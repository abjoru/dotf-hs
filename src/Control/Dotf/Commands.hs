module Control.Dotf.Commands (
  listTracked,
  listTrackedAll,
  listUntracked,
  unsafeListTracked,
  unsafeListUntracked,
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

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8

import           Data.Dotf                  (ErrorOrFilePaths, ErrorOrString,
                                             ErrorOrTracked,
                                             GitError (GitError),
                                             TrackedType (Staged, Tracked, Unstaged))
import           Data.String.Interpolate    (i)

import           System.Directory           (getHomeDirectory)
import           System.Exit                (ExitCode (..))
import           System.FilePath            ((</>))
import           System.Process.Typed       (ProcessConfig, proc, readProcess,
                                             runProcess, setWorkingDir)

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

listTrackedAll :: IO ErrorOrTracked
listTrackedAll = do
  tracked <- fmap (map Tracked) . processFileListResult <$> execGitTracked
  staged  <- fmap (map Staged) . processFileListResult <$> execGitTrackedStaged
  ustaged <- fmap (map Unstaged) . processFileListResult <$> execGitTrackedUnstaged
  return $ comb <$> tracked <*> staged <*> ustaged
  where comb a b c = a ++ b ++ c

listTracked :: IO ErrorOrTracked
listTracked = do
  staged <- fmap (map Staged) . processFileListResult<$> execGitTrackedStaged
  unstaged <- fmap (map Unstaged) . processFileListResult <$> execGitTrackedUnstaged
  return $ comb <$> staged <*> unstaged
  where comb a b = a ++ b

unsafeListTracked :: Bool -> IO [TrackedType]
unsafeListTracked True = fmap ignoreError listTrackedAll
unsafeListTracked False = fmap ignoreError listTracked

ignoreError :: ErrorOrTracked -> [TrackedType]
ignoreError (Right xs) = xs
ignoreError (Left _) = []

listUntracked :: IO ErrorOrFilePaths
listUntracked = processFileListResult <$> execGitUntracked

unsafeListUntracked :: IO [FilePath]
unsafeListUntracked = fmap unwrapList listUntracked
  where unwrapList (Right xs) = xs
        unwrapList (Left _)   = []

stageFile :: FilePath -> IO ExitCode
stageFile = execGitStageFile

unstageFile :: FilePath -> IO ExitCode
unstageFile = execGitUnstageFile

untrackFile :: FilePath -> IO ExitCode
untrackFile = execGitUntrackFile

diffFile :: FilePath -> IO ErrorOrString
diffFile fp = processStringResult <$> execGitDiffFile fp

commit :: String -> IO ExitCode
commit = execGitCommit

clone :: String -> IO ExitCode
clone = execGitCloneUrl

newBareRepo :: FilePath -> IO ExitCode
newBareRepo = execGitNewBareRepo

push :: IO ExitCode
push = execGitPush

pull :: IO ExitCode
pull = execGitPull

status :: IO ExitCode
status = execGitStatus

diffState :: IO ExitCode
diffState = execGitDiffStatus

---------------------
-- Implementations --
---------------------

execGitTracked :: IO ReadProcessResult
execGitTracked = bare ["ls-tree", "--name-only", "-r", "HEAD"] >>= readProcess

execGitTrackedStaged :: IO ReadProcessResult
execGitTrackedStaged = bare ["diff", "--name-only", "--cached"] >>= readProcess

execGitTrackedUnstaged :: IO ReadProcessResult
execGitTrackedUnstaged = bare ["diff", "--name-only"] >>= readProcess

execGitUntracked :: IO ReadProcessResult
execGitUntracked = bare ["ls-files", "--exclude-standard", "--others"] >>= readProcess

execGitStageFile :: FilePath -> IO ExitCode
execGitStageFile fp = bare ["add", fp] >>= runProcess

execGitUnstageFile :: FilePath -> IO ExitCode
execGitUnstageFile fp = bare ["reset", "--", fp] >>= runProcess

execGitUntrackFile :: FilePath -> IO ExitCode
execGitUntrackFile fp = bare ["rm", "--cached", fp] >>= runProcess

execGitDiffFile :: FilePath -> IO ReadProcessResult
execGitDiffFile fp = bare ["diff", fp] >>= readProcess

execGitCommit :: String -> IO ExitCode
execGitCommit msg = bare ["commit", "-m", msg] >>= runProcess

execGitCloneUrl :: String -> IO ExitCode
execGitCloneUrl url = bare ["clone", "--bare", url] >>= runProcess

execGitNewBareRepo :: FilePath -> IO ExitCode
execGitNewBareRepo fp = bare ["init", "--bare", fp] >>= runProcess

execGitPush :: IO ExitCode
execGitPush = bare ["push"] >>= runProcess

execGitPull :: IO ExitCode
execGitPull = bare ["pull"] >>= runProcess

execGitStatus :: IO ExitCode
execGitStatus = bare ["status", "-sb"] >>= runProcess

execGitDiffStatus :: IO ExitCode
execGitDiffStatus = bare ["diff"] >>= runProcess

bare :: [String] -> IO (ProcessConfig () () ())
bare args = do
  home <- getHomeDirectory
  return $ setWorkingDir home
         $ proc "git"
         $ [[i|--git-dir=#{home </> ".dotf"}|], [i|--work-tree=#{home}|]] ++ args

processFileListResult :: ReadProcessResult -> ErrorOrFilePaths
processFileListResult (ExitFailure cd, _, err) = Left $ GitError cd err
processFileListResult (ExitSuccess, out, _) = Right $ fmap C8.unpack (C8.lines out)

processStringResult :: ReadProcessResult -> ErrorOrString
processStringResult (ExitFailure cd, _, err) = Left $ GitError cd err
processStringResult (ExitSuccess, out, _)    = Right $ C8.unpack out
