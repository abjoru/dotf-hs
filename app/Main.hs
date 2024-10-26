module Main (main) where

import           Control.Monad.Extra (ifM, whenM)
import qualified Dotf.Commands       as CMD
import           Dotf.Options        (Command (..), Options (Options), readOpts)
import           Dotf.Templates      (missingRepoMessage)
import           Dotf.Types          (Answer (..))
import           System.Directory    (doesDirectoryExist, getHomeDirectory)
import           System.Environment  (getArgs)
import           System.FilePath     ((</>))
import           System.IO           (hFlush, stdout)
import           Tui                 (tui)

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
  | "init" `elem` args = readOpts >>= runApp
  | "new" `elem` args  = readOpts >>= runApp
  | otherwise = ifM checkBareRepo (readOpts >>= runApp) informMissingRepo

runApp :: Options -> IO ()
runApp (Options dry h Install)         = CMD.installBundles dry h
runApp (Options dry h Update)          = CMD.updateBundles dry h
runApp (Options dry _ (Init "abjoru")) = CMD.clone dry "git@github.com:abjoru/dotf.git"
runApp (Options dry _ (Init url))      = CMD.clone dry url
runApp (Options dry _ New)             = CMD.newBareRepo dry
runApp (Options dry _ Pull)            = CMD.pull dry
runApp (Options dry _ Push)            = CMD.push dry
runApp (Options dry _ (Commit m))      = CMD.commit dry m
runApp (Options dry _ Diff)            = CMD.diffState dry
runApp (Options dry _ Status)          = CMD.status dry
runApp (Options dry _ (Git cmd))       = CMD.gitRaw dry cmd

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
