module Main (main) where

import           Control.Monad.Extra (ifM, whenM)
import           Data.Char           (isLetter, toLower)
import           Dotf.Commands       (checkRequirements, commit, installBundles,
                                      installRequirements, pull, push)
import           Dotf.Options        (Command (Commit, Install, Pull, Push),
                                      DryMode (Dry, Normal), Options (Options),
                                      runOpts)
import           System.Environment  (getArgs)
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
bootstrap = ifM (checkRequirements False) (pure True) (doInstall >> pure False)

runArgs :: [String] -> IO ()
runArgs [] = tui
runArgs _  = runOpts >>= runApp

runApp :: Options -> IO ()
runApp (Options Dry Install)       = installBundles True
runApp (Options Normal Install)    = installBundles False
runApp (Options Dry Pull)          = pull True
runApp (Options Normal Pull)       = pull False
runApp (Options Dry Push)          = push True
runApp (Options Normal Push)       = push False
runApp (Options Dry (Commit m))    = commit True m
runApp (Options Normal (Commit m)) = commit False m
runApp v                           = print v

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
