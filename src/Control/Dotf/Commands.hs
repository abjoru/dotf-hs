module Control.Dotf.Commands (
  listBundles,
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
  diffState
) where

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8

import           Data.Dotf                  (Bundle, ErrorOrFilePaths,
                                             ErrorOrString, ErrorOrTracked,
                                             GitError (GitError),
                                             TrackedType (Staged, Tracked, Unstaged),
                                             appendToFile, listBundleFiles)
import           Data.String.Interpolate    (i)

import qualified Data.Yaml                  as Y
import           System.Directory           (doesFileExist, doesPathExist,
                                             getHomeDirectory)
import           System.FilePath            ((</>))
import           System.IO                  (IOMode (WriteMode), withFile)
import           System.Process             as P
import           System.Process.Typed       as PT

type ReadProcessResult = (ExitCode, B.ByteString, B.ByteString)

listBundles :: IO [Bundle]
listBundles = listBundleFiles >>= mapM Y.decodeFileThrow

listTrackedAll :: IO ErrorOrTracked
listTrackedAll = do
  tracked  <- fmap (map Tracked) . processFileListResult <$> execGitTracked
  rstaged  <- processFileListResult <$> execGitTrackedStaged
  rustaged <- processFileListResult <$> execGitTrackedUnstaged
  staged   <- mapEitherM Staged rstaged
  unstaged <- mapEitherM Unstaged rustaged
  return $ comb <$> tracked <*> staged <*> unstaged
  where comb a b c = a ++ b ++ c

listTracked :: IO ErrorOrTracked
listTracked = do
  rstaged   <- processFileListResult<$> execGitTrackedStaged
  runstaged <- processFileListResult <$> execGitTrackedUnstaged
  staged    <- mapEitherM Staged rstaged
  unstaged  <- mapEitherM Unstaged runstaged
  return $ comb <$> staged <*> unstaged
  where comb a b = a ++ b

unsafeListTracked :: Bool -> IO [TrackedType]
unsafeListTracked True  = fmap ignoreError listTrackedAll
unsafeListTracked False = fmap ignoreError listTracked

ignoreError :: ErrorOrTracked -> [TrackedType]
ignoreError (Right xs) = xs
ignoreError (Left _)   = []

listUntracked :: IO ErrorOrFilePaths
listUntracked = processFileListResult <$> execGitUntracked

unsafeListUntracked :: IO [FilePath]
unsafeListUntracked = fmap unwrapList listUntracked
  where unwrapList (Right xs) = xs
        unwrapList (Left _)   = []

ignoreFile :: FilePath -> IO ()
ignoreFile fp = getHomeDirectory >>= (appendToFile fp . target)
  where target home = home </> ".gitignore"

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
execGitTracked = bare ["ls-tree", "--name-only", "-r", "HEAD"] >>= PT.readProcess

execGitTrackedStaged :: IO ReadProcessResult
execGitTrackedStaged = bare ["diff", "--name-only", "--cached"] >>= PT.readProcess

execGitTrackedUnstaged :: IO ReadProcessResult
execGitTrackedUnstaged = bare ["diff", "--name-only"] >>= PT.readProcess

execGitUntracked :: IO ReadProcessResult
execGitUntracked = bare ["ls-files", "--exclude-standard", "--others"] >>= PT.readProcess

execGitStageFile :: FilePath -> IO ExitCode
execGitStageFile fp = bare ["add", fp] >>= PT.runProcess

execGitUnstageFile :: FilePath -> IO ExitCode
execGitUnstageFile fp = bare ["reset", "--", fp] >>= PT.runProcess

execGitUntrackFile :: FilePath -> IO ExitCode
execGitUntrackFile fp = do
  home <- getHomeDirectory
  exists <- doesFileExist $ home </> fp
  if exists
    then bare ["rm", "--cached", fp] >>= PT.runProcess
    else bareSilent ["rm", fp]

execGitDiffFile :: FilePath -> IO ReadProcessResult
execGitDiffFile fp = bare ["diff", fp] >>= PT.readProcess

execGitCommit :: String -> IO ExitCode
execGitCommit msg = bare ["commit", "-m", msg] >>= PT.runProcess

execGitCloneUrl :: String -> IO ExitCode
execGitCloneUrl url = bare ["clone", "--bare", url] >>= PT.runProcess

execGitNewBareRepo :: FilePath -> IO ExitCode
execGitNewBareRepo fp = bare ["init", "--bare", fp] >>= PT.runProcess

execGitPush :: IO ExitCode
execGitPush = bare ["push"] >>= PT.runProcess

execGitPull :: IO ExitCode
execGitPull = bare ["pull"] >>= PT.runProcess

execGitStatus :: IO ExitCode
execGitStatus = bare ["status", "-sb"] >>= PT.runProcess

execGitDiffStatus :: IO ExitCode
execGitDiffStatus = bare ["diff"] >>= PT.runProcess

bare :: [String] -> IO (ProcessConfig () () ())
bare args = do
  home <- getHomeDirectory
  return $ setWorkingDir home
         $ PT.proc "git"
         $ [[i|--git-dir=#{home </> ".dotf"}|], [i|--work-tree=#{home}|]] ++ args

bareSilent :: [String] -> IO ExitCode
bareSilent args = do
  home <- getHomeDirectory
  let newArgs = [[i|--git-dir=#{home </> ".dotf"}|], [i|--work-tree=#{home}|]] ++ args
  let nullDevice = "/dev/null"
  withFile nullDevice WriteMode $ \devNull -> do
    let spec = (P.proc "git" newArgs) { cwd = Just home
                                      , std_out = UseHandle devNull
                                      , std_err = UseHandle devNull
                                      }
    _ <- createProcess spec
    return ExitSuccess

processFileListResult :: ReadProcessResult -> ErrorOrFilePaths
processFileListResult (ExitFailure cd, _, err) = Left $ GitError cd err
processFileListResult (ExitSuccess, out, _) = Right $ fmap C8.unpack (C8.lines out)

processStringResult :: ReadProcessResult -> ErrorOrString
processStringResult (ExitFailure cd, _, err) = Left $ GitError cd err
processStringResult (ExitSuccess, out, _)    = Right $ C8.unpack out

mapEitherM :: (FilePath -> Bool -> TrackedType) -> Either e [FilePath] -> IO (Either e [TrackedType])
mapEitherM _ (Left err) = return (Left err)
mapEitherM f (Right v) = fmap Right (mapM checkPath v)
  where checkPath :: FilePath -> IO TrackedType
        checkPath p = do
          home <- getHomeDirectory
          f p <$> doesPathExist (home </> p)
