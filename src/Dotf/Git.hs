module Dotf.Git (
  gitTracked,
  gitTrackedStaged,
  gitTrackedUnstaged,
  gitUntracked,
  gitBare,
  gitBareSilent,
  processFileListResult,
  processStringResult,
  mapEitherM,
) where

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Function              ((&))
import           Dotf.Types                 (ErrorOrFilePaths, ErrorOrString,
                                             GitError (GitError), GitPackage,
                                             TrackedType)
import           Dotf.Utils                 (gitDir, workTree)
import           System.Directory           (doesPathExist, getHomeDirectory)
import           System.FilePath            ((</>))
import           System.IO                  (IOMode (WriteMode), openFile)
import qualified System.Process.Typed       as PT

type ReadProcessResult = (PT.ExitCode, B.ByteString, B.ByteString)

-- | Read all tracked GIT files.
gitTracked :: IO ReadProcessResult
gitTracked = gitBare ["ls-tree", "--name-only", "-r", "HEAD"] >>= PT.readProcess

-- | Read all GIT staged files.
gitTrackedStaged :: IO ReadProcessResult
gitTrackedStaged = gitBare ["diff", "--name-only", "--cached"] >>= PT.readProcess

-- | Read all GIT unstaged files.
gitTrackedUnstaged :: IO ReadProcessResult
gitTrackedUnstaged = gitBare ["diff", "--name-only"] >>= PT.readProcess

-- | Read all GIT untracked files.
gitUntracked :: IO ReadProcessResult
gitUntracked = gitBare ["ls-files", "--exclude-standard", "--others"] >>= PT.readProcess

-- | Execute a GIT command on the 'bare' repository.
gitBare :: [String] -> IO (PT.ProcessConfig () () ())
gitBare args = do
  home <- getHomeDirectory
  let newArgs = [gitDir home, workTree home] ++ args
  return $ PT.setWorkingDir home (PT.proc "git" newArgs)

-- | Alternative to `gitBare` which redirects output to /dev/null.
gitBareSilent :: [String] -> IO (PT.ProcessConfig () () ())
gitBareSilent args = do
  cfg     <- gitBare args
  devNull <- openFile "/dev/null" WriteMode
  return $ cfg & PT.setStdout (PT.useHandleClose devNull)
               & PT.setStderr (PT.useHandleClose devNull)

-- | Process file list results.
processFileListResult :: ReadProcessResult -> ErrorOrFilePaths
processFileListResult (PT.ExitFailure cd, _, err) = Left $ GitError cd err
processFileListResult (PT.ExitSuccess, out, _)    = Right $ fmap C8.unpack (C8.lines out)

-- | Process string results.
processStringResult :: ReadProcessResult -> ErrorOrString
processStringResult (PT.ExitFailure cd, _, err) = Left $ GitError cd err
processStringResult (PT.ExitSuccess, out, _)    = Right $ C8.unpack out

-- | Map an `Either` in the context of a monad.
-- The mapping function is passed the file path and a boolean
-- indicating if the file was staged or not, and must return
-- a `TrackedType` instance.
mapEitherM :: (FilePath -> Bool -> TrackedType) -> Either e [FilePath] -> IO (Either e [TrackedType])
mapEitherM _ (Left err) = return $ Left err
mapEitherM f (Right v)  = fmap Right (mapM checkPath v)
 where checkPath p = do
         home <- getHomeDirectory
         f p <$> doesPathExist (home </> p)
