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
import           Dotf.Git
import           Dotf.Types           (Distro (Arch, Osx, Unsupported), Dry,
                                       ErrorOrFilePaths, ErrorOrString,
                                       ErrorOrTracked, GitPackage, Package,
                                       TrackedType (..))
import           Dotf.Utils           (appendToFile, distro, gitIgnoreFile,
                                       resolveScript, which)
import           System.Directory     (createDirectoryIfMissing,
                                       doesDirectoryExist, doesFileExist,
                                       getHomeDirectory)
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
stageFile fp = gitBare ["add", fp] >>= PT.runProcess

unstageFile :: FilePath -> IO ExitCode
unstageFile fp = gitBareSilent ["reset", "--", fp] >>= PT.runProcess

untrackFile :: FilePath -> IO ExitCode
untrackFile fp = do
  home <- getHomeDirectory
  exists <- doesFileExist $ home </> fp
  if exists
    then gitBare ["rm", "--cached", fp] >>= PT.runProcess
    else gitBareSilent ["rm", fp] >>= PT.runProcess

diffFile :: FilePath -> IO ErrorOrString
diffFile fp = do
  res <- gitBare ["diff", fp] >>= PT.readProcess
  return $ processStringResult res

commit :: Dry -> String -> IO ()
commit True msg  = gitBare ["commit", "-m", msg] >>= print
commit False msg = gitBare ["commit", "-m", msg] >>= (void . PT.runProcess)

commit' :: String -> IO ()
commit' msg = gitBareSilent ["commit", "-m", msg] >>= (void . PT.runProcess)

clone :: Dry -> String -> IO ()
clone d url = do
  home <- getHomeDirectory
  cfg1 <- gitBare ["clone", "--bare", url, home </> ".dotf"]
  cfg2 <- gitBare ["checkout"]
  runCmd d cfg1 cfg2
  where runCmd True a b  = print a >> print b
        runCmd False a b = void $ PT.runProcess a >> PT.runProcess b

newBareRepo :: Dry -> IO ()
newBareRepo dry = getHomeDirectory >>= mkCmd >>= if dry then print else void . PT.runProcess
  where mkCmd h = gitBare ["init", "--bare", h </> ".dotf"]

push :: Dry -> IO ()
push False = gitBare ["push"] >>= (void . PT.runProcess)
push True  = gitBare ["push"] >>= print

pull :: Dry -> IO ()
pull False = gitBare ["pull"] >>= (void . PT.runProcess)
pull True  = gitBare ["pull"] >>= print

status :: Dry -> IO ()
status False = gitBare ["status", "-sb"] >>= (void . PT.runProcess)
status True  = gitBare ["status", "-sb"] >>= print

diffState :: Dry -> IO ()
diffState False = gitBare ["diff"] >>= (void . PT.runProcess)
diffState True  = gitBare ["diff"] >>= print

gitRaw :: Dry -> String -> IO ()
gitRaw False cmd = gitBare (words cmd) >>= (void . PT.runProcess)
gitRaw True cmd  = gitBare (words cmd) >>= print

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
