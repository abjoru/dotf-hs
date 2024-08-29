module Control.Dotf.Commands.Git (
  gitTracked,
  gitTrackedStaged,
  gitTrackedUnstaged,
  gitUntracked,
  gitStageFile,
  gitUnstageFile,
  gitUntrackFile,
  gitDiffFile,
  gitCommit,
  gitCloneBareUrl,
  gitNewBareRepo,
  gitPush,
  gitPull,
  gitStatus,
  gitDiffStatus,
  gitIgnoreFile,
  processFileListResult,
  processStringResult,
  mapEitherM,
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Dotf (ErrorOrFilePaths, ErrorOrString, GitError (GitError), TrackedType)
import Data.String.Interpolate (i)
import System.Directory (doesFileExist, doesPathExist, getHomeDirectory)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), withFile)
import qualified System.Process as P
import qualified System.Process.Typed as PT

type ReadProcessResult = (PT.ExitCode, B.ByteString, B.ByteString)

gitTracked :: IO ReadProcessResult
gitTracked = bare ["ls-tree", "--name-only", "-r", "HEAD"] >>= PT.readProcess

gitTrackedStaged :: IO ReadProcessResult
gitTrackedStaged = bare ["diff", "--name-only", "--cached"] >>= PT.readProcess

gitTrackedUnstaged :: IO ReadProcessResult
gitTrackedUnstaged = bare ["diff", "--name-only"] >>= PT.readProcess

gitUntracked :: IO ReadProcessResult
gitUntracked = bare ["ls-files", "--exclude-standard", "--others"] >>= PT.readProcess

gitStageFile :: FilePath -> IO PT.ExitCode
gitStageFile fp = bare ["add", fp] >>= PT.runProcess

gitUnstageFile :: FilePath -> IO PT.ExitCode
gitUnstageFile fp = bare ["reset", "--", fp] >>= PT.runProcess

gitUntrackFile :: FilePath -> IO PT.ExitCode
gitUntrackFile fp = do
  home <- getHomeDirectory
  exists <- doesFileExist $ home </> fp
  if exists
    then bare ["rm", "--cached", fp] >>= PT.runProcess
    else bareSilent ["rm", fp]

gitDiffFile :: FilePath -> IO ReadProcessResult
gitDiffFile fp = bare ["diff", fp] >>= PT.readProcess

gitCommit :: String -> IO PT.ExitCode
gitCommit msg = bare ["commit", "-m", msg] >>= PT.runProcess

gitCloneBareUrl :: String -> IO PT.ExitCode
gitCloneBareUrl url = bare ["clone", "--bare", url] >>= PT.runProcess

gitNewBareRepo :: FilePath -> IO PT.ExitCode
gitNewBareRepo fp = bare ["init", "--bare", fp] >>= PT.runProcess

gitPush :: IO PT.ExitCode
gitPush = bare ["push"] >>= PT.runProcess

gitPull :: IO PT.ExitCode
gitPull = bare ["pull"] >>= PT.runProcess

gitStatus :: IO PT.ExitCode
gitStatus = bare ["status", "-sb"] >>= PT.runProcess

gitDiffStatus :: IO PT.ExitCode
gitDiffStatus = bare ["diff"] >>= PT.runProcess

-----------
-- Utils --
-----------

gitIgnoreFile :: FilePath -> FilePath
gitIgnoreFile base = base </> ".gitignore"

gitDir :: FilePath -> String
gitDir homeDir = [i|--git-dir=#{homeDir </> ".dotf"}|]

workTree :: FilePath -> String
workTree homeDir = [i|--work-tree=#{homeDir}|]

bare :: [String] -> IO (PT.ProcessConfig () () ())
bare args = do
  home <- getHomeDirectory
  let newArgs = [gitDir home, workTree home] ++ args
  return $ PT.setWorkingDir home (PT.proc "git" newArgs)

bareSilent :: [String] -> IO PT.ExitCode
bareSilent args = do
  home <- getHomeDirectory
  let newArgs = [gitDir home, workTree home] ++ args
      nullDev = "/dev/null"
  withFile nullDev WriteMode $ \devNull -> do
    let spec =
          (P.proc "git" newArgs)
            { P.cwd = Just home
            , P.std_out = P.UseHandle devNull
            , P.std_err = P.UseHandle devNull
            }
    _ <- P.createProcess spec
    return PT.ExitSuccess

processFileListResult :: ReadProcessResult -> ErrorOrFilePaths
processFileListResult (PT.ExitFailure cd, _, err) = Left $ GitError cd err
processFileListResult (PT.ExitSuccess, out, _) = Right $ fmap C8.unpack (C8.lines out)

processStringResult :: ReadProcessResult -> ErrorOrString
processStringResult (PT.ExitFailure cd, _, err) = Left $ GitError cd err
processStringResult (PT.ExitSuccess, out, _) = Right $ C8.unpack out

mapEitherM :: (FilePath -> Bool -> TrackedType) -> Either e [FilePath] -> IO (Either e [TrackedType])
mapEitherM _ (Left err) = return $ Left err
mapEitherM f (Right v) = fmap Right (mapM checkPath v)
 where
  checkPath p = do
    home <- getHomeDirectory
    f p <$> doesPathExist (home </> p)
