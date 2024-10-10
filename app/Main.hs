module Main (main) where

import           Control.Monad.Extra (ifM, whenM)
import           Data.Char           (isLetter, toLower)
import           Dotf.Commands       (checkRequirements, clone, commit,
                                      installBundles, installRequirements,
                                      newBareRepo, pull, push)
import           Dotf.Git            (gitDiffStatus, gitStatus)
import           Dotf.Options        (Command (Commit, Diff, Init, Install, New, Pull, Push, Status),
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
bootstrap = ifM checkRequirements (pure True) (doInstall >> pure False)

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
runApp (Options Dry Install)          = installBundles True
runApp (Options Normal Install)       = installBundles False
runApp (Options Dry (Init url))       = clone True url
runApp (Options Normal (Init "dotf")) = clone False "git@github.com:abjoru/dotf.git"
runApp (Options Normal (Init url))    = clone False url
runApp (Options Dry New)              = newBareRepo True
runApp (Options Normal New)           = newBareRepo False
runApp (Options Dry Pull)             = pull True
runApp (Options Normal Pull)          = pull False
runApp (Options Dry Push)             = push True
runApp (Options Normal Push)          = push False
runApp (Options Dry (Commit m))       = commit True m
runApp (Options Normal (Commit m))    = commit False m
runApp (Options Dry Diff)             = gitDiffStatus True
runApp (Options Normal Diff)          = gitDiffStatus False
runApp (Options Dry Status)           = gitStatus True
runApp (Options Normal Status)        = gitStatus False
runApp v                              = print v

doInstall :: IO ()
doInstall = askInstall >>= check
  where check Yes    = installRequirements False
        check DryRun = installRequirements True
        check _      = pure ()

askInstall :: IO Answer
askInstall = do
  putStrLn "Missing dependencies!"
  putStr "Do you want to install them? (d(ry)/y(es)/N(o)) "
  hFlush stdout
  read <$> getLine

informMissingRepo :: IO ()
informMissingRepo = putStrLn missingRepoMessage
