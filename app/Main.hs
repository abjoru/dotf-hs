module Main (main) where

import           Control.Monad.Extra (ifM, whenM)
import           Data.Char           (isLetter, toLower)
import qualified Dotf.Commands       as CMD
import           Dotf.Options        (Command (Commit, Diff, Git, Init, Install, New, Pull, Push, Status),
                                      DryMode (Dry, Normal), Options (Options),
                                      runOpts)
import           Dotf.Templates      (missingRepoMessage)
import           System.Directory    (doesDirectoryExist, getHomeDirectory)
import           System.Environment  (getArgs)
import           System.FilePath     ((</>))
import           System.IO           (hFlush, stdout)
import           Tui                 (tui)

data Answer = Yes | No | DryRun
  deriving Show

instance Read Answer where
  readsPrec _ input = case fmap toLower . filter isLetter $ input of
    "yes" -> [(Yes, [])]
    "y"   -> [(Yes, [])]
    "dry" -> [(DryRun, [])]
    "d"   -> [(DryRun, [])]
    _     -> [(No, [])]

main :: IO ()
main = whenM bootstrap $ getArgs >>= runArgs

bootstrap :: IO Bool
bootstrap = ifM CMD.checkRequirements (pure True) (doInstall >> pure False)

checkBareRepo :: IO Bool
checkBareRepo = do
  home <- getHomeDirectory
  doesDirectoryExist $ home </> ".dotf"

runArgs :: [String] -> IO ()
runArgs [] = ifM checkBareRepo tui informMissingRepo
runArgs args
  | "init" `elem` args = runOpts >>= runApp
  | "new" `elem` args  = runOpts >>= runApp
  | otherwise = ifM checkBareRepo (runOpts >>= runApp) informMissingRepo

runApp :: Options -> IO ()
runApp (Options Dry Install)          = CMD.installBundles True
runApp (Options Normal Install)       = CMD.installBundles False
runApp (Options Dry (Init url))       = CMD.clone True url
runApp (Options Normal (Init "dotf")) = CMD.clone False "git@github.com:abjoru/dotf.git"
runApp (Options Normal (Init url))    = CMD.clone False url
runApp (Options Dry New)              = CMD.newBareRepo True
runApp (Options Normal New)           = CMD.newBareRepo False
runApp (Options Dry Pull)             = CMD.pull True
runApp (Options Normal Pull)          = CMD.pull False
runApp (Options Dry Push)             = CMD.push True
runApp (Options Normal Push)          = CMD.push False
runApp (Options Dry (Commit m))       = CMD.commit True m
runApp (Options Normal (Commit m))    = CMD.commit False m
runApp (Options Dry Diff)             = CMD.diffState True
runApp (Options Normal Diff)          = CMD.diffState False
runApp (Options Dry Status)           = CMD.status True
runApp (Options Normal Status)        = CMD.status False
runApp (Options Dry (Git cmd))        = CMD.gitRaw True cmd
runApp (Options Normal (Git cmd))     = CMD.gitRaw False cmd

doInstall :: IO ()
doInstall = askInstall >>= check
  where check Yes    = CMD.installRequirements False
        check DryRun = CMD.installRequirements True
        check _      = pure ()

askInstall :: IO Answer
askInstall = do
  putStrLn "Missing dependencies!"
  putStr "Do you want to install them? (d(ry)/y(es)/N(o)) "
  hFlush stdout
  read <$> getLine

informMissingRepo :: IO ()
informMissingRepo = putStrLn missingRepoMessage
