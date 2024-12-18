module Dotf.Options (
  Command(..),
  Options(..),

  readOpts
) where

import           Data.String.Interpolate (__i)
import           Data.Version            (showVersion)
import           Dotf.Types              (Dry, Headless)
import           Options.Applicative     (Parser, argument, command, execParser,
                                          flag, fullDesc, help, helper,
                                          hsubparser, info, long, metavar,
                                          progDesc, short, str, (<**>))
import           Paths_dotf_hs           (version)

-----------
-- Types --
-----------

data Command
  = Install
  | Update
  | Init String
  | New
  | Pull
  | Push
  | Commit String
  | Diff
  | Status
  | Git String
  deriving Show

data Options = Options {
  dry      :: Dry,
  headless :: Headless,
  cmd      :: Command
} deriving Show

-------------
-- Parsers --
-------------

parseDryMode :: Parser Dry
parseDryMode = flag False True
  (  long "dryrun"
  <> short 'd'
  <> help "Enable dry mode"
  )

parseHeadless :: Parser Headless
parseHeadless = flag False True
  (  long "headless"
  <> help "Install headless system"
  )

parseCommand :: Parser Command
parseCommand = hsubparser
  (  command "install" (info (pure Install) (progDesc "Install bundles"))
  <> command "update" (info (pure Update) (progDesc "Update os/git packages"))
  <> command "init" (info parseInit (progDesc "Initialize dot-files bare repo"))
  <> command "new" (info (pure New) (progDesc "Create a new bare repository"))
  <> command "pull" (info (pure Pull) (progDesc "Pull from upstream"))
  <> command "push" (info (pure Push) (progDesc "Push changes upstream"))
  <> command "commit" (info parseCommit (progDesc "Commit all changes"))
  <> command "diff" (info (pure Diff) (progDesc "Show file diff for dot-files"))
  <> command "status" (info (pure Status) (progDesc "Show file status for dot-files"))
  <> command "git" (info parseGit (progDesc "Run raw git commands"))
  )

parseInit :: Parser Command
parseInit = Init <$> argument str (metavar "GIT_URL")

parseCommit :: Parser Command
parseCommit = Commit <$> argument str (metavar "COMMIT_MSG")

parseGit :: Parser Command
parseGit = Git <$> argument str (metavar "GIT_OPTS")

parseOptions :: Parser Options
parseOptions = Options <$> parseDryMode <*> parseHeadless <*> parseCommand

-------------
-- Methods --
-------------

-- | Executes the `Options` parser on the program args.
readOpts :: IO Options
readOpts = execParser $ info (parseOptions <**> helper)
  (fullDesc <> progDesc [__i|DotF #{showVersion version} :: The simple dot-file manager.

                             This little application allows for setting up standard applications that
                             should be installed on a new system. It wraps some GIT commands to make
                             it easier to work with a bare repository.
                        |])
