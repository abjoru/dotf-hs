module Dotf.Options (
  DryMode(..),
  Command(..),
  Options(..),

  runOpts
) where

import           Data.String.Interpolate (__i)
import           Options.Applicative     (Parser, argument, command, execParser,
                                          flag, fullDesc, help, helper,
                                          hsubparser, info, long, metavar,
                                          progDesc, short, str, strOption,
                                          (<**>))

-----------
-- Types --
-----------

data DryMode = Normal | Dry deriving Show

data Command
  = Install
  | Init String
  | New
  | Pull
  | Push
  | Commit String
  | Git String
  deriving Show

data Options = Options {
  dry :: DryMode,
  cmd :: Command
} deriving Show

-------------
-- Parsers --
-------------

parseDryMode :: Parser DryMode
parseDryMode = flag Normal Dry
  (  long "dryrun"
  <> short 'd'
  <> help "Enable dry mode"
  )

parseCommand :: Parser Command
parseCommand = hsubparser
  (  command "install" (info (pure Install) (progDesc "Install bundles"))
  <> command "init" (info parseInit (progDesc "Initialize dot-files bare repo"))
  <> command "new" (info (pure New) (progDesc "Create a new bare repository"))
  <> command "pull" (info (pure Pull) (progDesc "Pull from upstream"))
  <> command "push" (info (pure Push) (progDesc "Push changes upstream"))
  <> command "commit" (info parseCommit (progDesc "Commit all changes"))
  <> command "git" (info parseGit (progDesc "Run raw git commands"))
  )

parseInit :: Parser Command
parseInit = Init <$> strOption
  (  long "url"
  <> short 'u'
  <> metavar "GIT_URL"
  <> help "Upstream GIT bare repository URL"
  )

parseCommit :: Parser Command
parseCommit = Commit <$> strOption
  (  long "message"
  <> short 'm'
  <> metavar "COMMIT_MSG"
  <> help "Message for commit"
  )

parseGit :: Parser Command
parseGit = Git <$> argument str (metavar "GIT_OPTS")

parseOptions :: Parser Options
parseOptions = Options <$> parseDryMode <*> parseCommand

-------------
-- Methods --
-------------

runOpts :: IO Options
runOpts = execParser $ info (parseOptions <**> helper)
  (fullDesc <> progDesc [__i|DotF :: The simple dot-file manager.

                             This little application allows for setting up standard applications that
                             should be installed on a new system. It wraps some GIT commands to make
                             it easier to work with a bare repository.
                        |])
