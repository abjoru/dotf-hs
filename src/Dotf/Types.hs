module Dotf.Types (
  Distro(..),
  Package(..),
  NamedPackage(..),
  GitPackage(..),
  Bundle(..),
  TrackedType(..),
  GitError(..),
  Answer(..),
  AppCategory(..),
  AppLauncher(..),
  AppConfig(..),

  Dry,
  Headless,
  ErrorOrString,
  ErrorOrFilePaths,
  ErrorOrTracked,
  ErrorOrBundles,
  ErrorOrAppConfig,

  Path(..),

  emptyAppConfig
) where

import           Data.Aeson.Key             (toString, fromString)
import           Data.Aeson.KeyMap          (keys)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Char                  (isLetter, toLower)
import           Data.Yaml                  (FromJSON (parseJSON), withObject,
                                             (.!=), (.:), (.:?), ParseException)
import qualified Data.Yaml                  as Y

-----------
-- Types --
-----------

data Distro = Arch | Osx | Deb | Unsupported

data Package = Package {
  arch :: Maybe String,
  osx  :: Maybe String,
  deb  :: Maybe String
} deriving (Show, Eq, Ord)

data NamedPackage = NamedPackage String Package
  deriving (Show, Eq, Ord)

data GitPackage = GitPackage {
  gitName        :: String,
  gitSkip        :: [String],
  gitUrl         :: String,
  gitBranch      :: Maybe String,
  gitSubmodules  :: Bool,
  gitInstallCmd  :: Maybe String,
  gitInstallPath :: Maybe FilePath
} deriving (Show, Eq, Ord)

data Bundle = Bundle {
  bundleName              :: String,
  bundleHeadless          :: Bool,
  bundleOsPackages        :: [NamedPackage],
  bundleGitPackages       :: [GitPackage],
  bundlePreInstallScript  :: Maybe FilePath,
  bundlePostInstallScript :: Maybe FilePath
} deriving (Show, Eq, Ord)

data TrackedType
  = Tracked FilePath
  | Staged FilePath Bool
  | Unstaged FilePath Bool
  deriving (Eq)

data GitError = GitError {
  errorCode:: Int,
  errorMessage :: B.ByteString
} deriving (Eq)

type Dry = Bool

type Headless = Bool

type ErrorOrString = Either GitError String

type ErrorOrFilePaths = Either GitError [FilePath]

type ErrorOrTracked = Either GitError [TrackedType]

type ErrorOrBundles = Either ParseException [Bundle]

type ErrorOrAppConfig = Either ParseException AppConfig

data Answer = Yes | No | DryRun
  deriving Show

data AppCategory
  = CFavorites
  | CGames
  | CInternet
  | CSettings
  | CSystem
  | COffice
  deriving (Eq, Ord)

data AppLauncher = AppLauncher {
  appName        :: String,
  appDescription :: String
} deriving (Show, Eq, Ord)

data AppConfig = AppConfig {
  _favorites :: [AppLauncher],
  _games     :: [AppLauncher],
  _internet  :: [AppLauncher],
  _settings  :: [AppLauncher],
  _system    :: [AppLauncher],
  _office    :: [AppLauncher]
} deriving (Show, Eq, Ord)

-----------------
-- Typeclasses --
-----------------

class Path a where
  toPath :: a -> FilePath

---------------
-- Instances --
---------------

instance Read Answer where
  readsPrec _ input = case fmap toLower . filter isLetter $ input of
    "yes" -> [(Yes, [])]
    "y"   -> [(Yes, [])]
    "dry" -> [(DryRun, [])]
    "d"   -> [(DryRun, [])]
    _     -> [(No, [])]

instance Show TrackedType where
  show (Tracked fp)        = fp
  show (Staged fp True)    = fp
  show (Staged fp False)   = "(D) " ++ fp
  show (Unstaged fp True)  = fp
  show (Unstaged fp False) = "(D) " ++ fp

instance Show AppCategory where
  show CFavorites = "favorites"
  show CGames     = "games"
  show CInternet  = "internet"
  show CSettings  = "settings"
  show CSystem    = "system"
  show COffice    = "office"

instance Path TrackedType where
  toPath (Tracked fp)    = fp
  toPath (Staged fp _)   = fp
  toPath (Unstaged fp _) = fp

instance Show GitError where
  show err = concat ["Error occurred: ", show (errorCode err), " - ", C8.unpack (errorMessage err)]

instance FromJSON Package where
  parseJSON (Y.Object o) =
    Package <$> o .:? "arch"
            <*> o .:? "osx"
            <*> o .:? "deb"
  parseJSON v = fail $ "Expected Object for Package value: " ++ show v

instance FromJSON NamedPackage where
  parseJSON = withObject "NamedPackage" $ \v -> do
    let key = head (keys v)
    val <- v .: key
    pkg <- parseJSON val
    return $ NamedPackage (toString key) pkg

instance FromJSON GitPackage where
  parseJSON (Y.Object o) =
    GitPackage <$> o .: "name"
               <*> o .:? "ignore" .!= []
               <*> o .: "url"
               <*> o .:? "branch"
               <*> o .:? "submodules" .!= False
               <*> o .:? "install-cmd"
               <*> o .:? "install-path"
  parseJSON _ = fail "Expected Object for GitPackage value"

instance FromJSON Bundle where
  parseJSON (Y.Object o) =
    Bundle <$> o .: "name"
           <*> o .:? "headless" .!= False
           <*> o .:? "os-packages" .!= []
           <*> o .:? "git-packages" .!= []
           <*> o .:? "pre-install"
           <*> o .:? "post-install"
  parseJSON _ = fail "Expected Object for Bundle value"

instance FromJSON AppLauncher where
  parseJSON = withObject "AppLauncher" $ \v -> do
    AppLauncher <$> v .: "name"
                <*> v .: "description"

instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \v -> do
    AppConfig <$> v .: fromString (show CFavorites)
              <*> v .: fromString (show CGames)
              <*> v .: fromString (show CInternet)
              <*> v .: fromString (show CSettings)
              <*> v .: fromString (show CSystem)
              <*> v .: fromString (show COffice)

---------------
-- Functions --
---------------

emptyAppConfig :: AppConfig
emptyAppConfig = AppConfig [] [] [] [] [] []
