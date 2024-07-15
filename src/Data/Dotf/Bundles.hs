module Data.Dotf.Bundles (
  Package(..),
  NamedPackage(..),
  GitPackage(..),
  Bundle(..),

  loadBundles,
  packages,
  scripts,
  osPackages,
  collectPackages,
  collectOsPackages,
  collectNamedPackages,
  collectGitPackages,
  collectScripts
) where

import           Control.Monad     (foldM)

import           Data.Aeson.Key    (toString)
import           Data.Aeson.KeyMap (keys)
import           Data.Dotf.Os      (Distro (..), distro, listBundleFiles)
import           Data.Yaml         (FromJSON (..), withObject, (.!=), (.:),
                                    (.:?))
import qualified Data.Yaml         as Y

data Package = Package {
  arch :: Maybe String,
  osx  :: Maybe String,
  deb  :: Maybe String
} deriving (Show, Eq, Ord)

data NamedPackage = NamedPackage String Package
  deriving (Show, Eq, Ord)

data GitPackage = GitPackage {
  gitName       :: String,
  gitUrl        :: String,
  gitBranch     :: Maybe String,
  gitSubmodules :: Bool,
  gitInstallCmd :: Maybe String
} deriving (Show, Eq, Ord)

data Bundle = Bundle {
  bundleName              :: String,
  bundleHeadless          :: Bool,
  bundleOsPackages        :: [NamedPackage],
  bundleGitPackages       :: [GitPackage],
  bundlePreInstallScript  :: Maybe FilePath,
  bundlePostInstallScript :: Maybe FilePath
} deriving (Show, Eq, Ord)

----------------
-- Operations --
----------------

loadBundles :: IO [Bundle]
loadBundles = listBundleFiles >>= mapM Y.decodeFileThrow

osPackages :: Bundle -> IO [String]
osPackages bundle = fmap (\d -> foldr (find d) [] $ packages bundle) distro
  where find Arch (Package (Just p) _ _) xs = p : xs
        find Osx (Package _ (Just p) _) xs  = p : xs
        find Deb (Package _ _ (Just p)) xs  = p : xs
        find _ _ xs                         = xs

packages :: Bundle -> [Package]
packages b = map (\(NamedPackage _ p) -> p) $ bundleOsPackages b

scripts :: Bundle -> [FilePath]
scripts v = mkScriptList (bundlePreInstallScript v) (bundlePostInstallScript v)
  where mkScriptList (Just a) (Just b) = [a, b]
        mkScriptList (Just a) Nothing  = [a]
        mkScriptList Nothing (Just b)  = [b]
        mkScriptList _ _               = []

collectPackages :: [Bundle] -> [Package]
collectPackages = foldr (\b acc -> packages b ++ acc) []

collectNamedPackages :: [Bundle] -> [NamedPackage]
collectNamedPackages = foldr (\b acc -> bundleOsPackages b ++ acc) []

collectOsPackages :: [Bundle] -> IO [String]
collectOsPackages = foldM (\acc b -> (++ acc) <$> osPackages b) []

collectGitPackages :: [Bundle] -> [GitPackage]
collectGitPackages = foldr (\b acc -> bundleGitPackages b ++ acc) []

collectScripts :: [Bundle] -> [FilePath]
collectScripts = foldr (\b acc -> scripts b ++ acc) []

-----------------
-- YAML Parses --
-----------------

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
               <*> o .: "url"
               <*> o .:? "branch"
               <*> o .:? "submodules" .!= False
               <*> o .:? "install-cmd"
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
