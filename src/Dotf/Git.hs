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
import           Data.Function
import           Dotf.Types                 (ErrorOrFilePaths, ErrorOrString,
                                             GitError (GitError), TrackedType)
import           Dotf.Utils
import           System.Directory           (doesPathExist, getHomeDirectory)
import           System.FilePath            ((</>))
import           System.IO                  (IOMode (WriteMode), openFile)
import qualified System.Process.Typed       as PT

type ReadProcessResult = (PT.ExitCode, B.ByteString, B.ByteString)

gitTracked :: IO ReadProcessResult
gitTracked = gitBare ["ls-tree", "--name-only", "-r", "HEAD"] >>= PT.readProcess

gitTrackedStaged :: IO ReadProcessResult
gitTrackedStaged = gitBare ["diff", "--name-only", "--cached"] >>= PT.readProcess

gitTrackedUnstaged :: IO ReadProcessResult
gitTrackedUnstaged = gitBare ["diff", "--name-only"] >>= PT.readProcess

gitUntracked :: IO ReadProcessResult
gitUntracked = gitBare ["ls-files", "--exclude-standard", "--others"] >>= PT.readProcess

gitBare :: [String] -> IO (PT.ProcessConfig () () ())
gitBare args = do
  home <- getHomeDirectory
  let newArgs = [gitDir home, workTree home] ++ args
  return $ PT.setWorkingDir home (PT.proc "git" newArgs)

gitBareSilent :: [String] -> IO (PT.ProcessConfig () () ())
gitBareSilent args = do
  cfg <- gitBare args
  devNull <- openFile "/dev/null" WriteMode
  return $ cfg & PT.setStdout (PT.useHandleClose devNull)
               & PT.setStderr (PT.useHandleClose devNull)

processFileListResult :: ReadProcessResult -> ErrorOrFilePaths
processFileListResult (PT.ExitFailure cd, _, err) = Left $ GitError cd err
processFileListResult (PT.ExitSuccess, out, _) = Right $ fmap C8.unpack (C8.lines out)

processStringResult :: ReadProcessResult -> ErrorOrString
processStringResult (PT.ExitFailure cd, _, err) = Left $ GitError cd err
processStringResult (PT.ExitSuccess, out, _)    = Right $ C8.unpack out

mapEitherM :: (FilePath -> Bool -> TrackedType) -> Either e [FilePath] -> IO (Either e [TrackedType])
mapEitherM _ (Left err) = return $ Left err
mapEitherM f (Right v)  = fmap Right (mapM checkPath v)
 where checkPath p = do
         home <- getHomeDirectory
         f p <$> doesPathExist (home </> p)
