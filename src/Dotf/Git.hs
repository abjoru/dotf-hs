module Dotf.Git (
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
  gitCheckoutBare,
  gitPush,
  gitPull,
  gitStatus,
  gitDiffStatus,
  gitIgnoreFile,
  processFileListResult,
  processStringResult,
  mapEitherM,
) where

import           Control.Monad              (void)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8
import           Dotf.Types                 (Dry, ErrorOrFilePaths,
                                             ErrorOrString, GitError (GitError),
                                             TrackedType)
import           Dotf.Utils
import           System.Directory           (doesFileExist, doesPathExist,
                                             getHomeDirectory)
import           System.FilePath            ((</>))
import           System.IO                  (IOMode (WriteMode), withFile)
import qualified System.Process             as P
import qualified System.Process.Typed       as PT

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
  home   <- getHomeDirectory
  exists <- doesFileExist $ home </> fp
  if exists
    then bare ["rm", "--cached", fp] >>= PT.runProcess
    else bareSilent ["rm", fp]

gitDiffFile :: FilePath -> IO ReadProcessResult
gitDiffFile fp = bare ["diff", fp] >>= PT.readProcess

gitCommit :: Dry -> String -> IO ()
gitCommit False msg = bare ["commit", "-m", msg] >>= (void . PT.runProcess)
gitCommit True msg  = bare ["commit", "-m", msg] >>= print

gitCloneBareUrl :: Dry -> String -> IO ()
gitCloneBareUrl False url = do
  home <- getHomeDirectory
  bare ["clone", "--bare", url, home </> ".dotf"] >>= (void . PT.runProcess)
gitCloneBareUrl True url = do
  home <- getHomeDirectory
  bare ["clone", "--bare", url, home </> ".dotf"] >>= print

gitCheckoutBare :: Dry -> IO ()
gitCheckoutBare True  = bare ["checkout"] >>= print
gitCheckoutBare False = bare ["checkout"] >>= (void . PT.runProcess)

gitNewBareRepo :: Dry -> FilePath -> IO ()
gitNewBareRepo False fp = bare ["init", "--bare", fp] >>= (void . PT.runProcess)
gitNewBareRepo True fp  = bare ["init", "--bare", fp] >>= print

gitPush :: Dry -> IO ()
gitPush False = bare ["push"] >>= (void . PT.runProcess)
gitPush True  = bare ["push"] >>= print

gitPull :: Dry -> IO ()
gitPull False = bare ["pull"] >>= (void . PT.runProcess)
gitPull True  = bare ["pull"] >>= print

gitStatus :: Dry -> IO ()
gitStatus False = bare ["status", "-sb"] >>= (void . PT.runProcess)
gitStatus True  = bare ["status", "-sb"] >>= print

gitDiffStatus :: Dry -> IO ()
gitDiffStatus False = bare ["diff"] >>= (void . PT.runProcess)
gitDiffStatus True  = bare ["diff"] >>= print

-----------
-- Utils --
-----------

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
processStringResult (PT.ExitSuccess, out, _)    = Right $ C8.unpack out

mapEitherM :: (FilePath -> Bool -> TrackedType) -> Either e [FilePath] -> IO (Either e [TrackedType])
mapEitherM _ (Left err) = return $ Left err
mapEitherM f (Right v)  = fmap Right (mapM checkPath v)
 where checkPath p = do
         home <- getHomeDirectory
         f p <$> doesPathExist (home </> p)
