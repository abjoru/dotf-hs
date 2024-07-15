module Data.Dotf.Os (
  Distro(..),
  distro,

  listBundleFiles,
  listFilesInDir,
  appendToFile
) where

import           Control.Monad    (filterM)
import           System.Directory (XdgDirectory (XdgConfig), doesDirectoryExist,
                                   doesFileExist, getXdgDirectory,
                                   listDirectory)
import           System.FilePath  (takeExtension, (</>))
import           System.Info      (os)
import           System.IO        (IOMode (WriteMode), withFile)
import           System.OsRelease (OsRelease (name),
                                   OsReleaseResult (osRelease), parseOsRelease)

data Distro = Arch | Osx | Deb | Unsupported
  deriving Show

distro :: IO Distro
distro = case os of
  "linux"  -> maybe Unsupported (findName . osRelease) <$> parseOsRelease
  "darwin" -> pure Osx
  _        -> pure Unsupported

findName :: OsRelease -> Distro
findName o = case name o of
  "Arch Linux" -> Arch
  "Ubuntu"     -> Deb
  "Debian"     -> Deb
  _            -> Unsupported

listBundleFiles :: IO [FilePath]
listBundleFiles = do
  cfgDir <- getXdgDirectory XdgConfig "dotf"
  listFilesInDir (cfgDir </> "bundles") yamlFilter
  where yamlFilter fp = let ext = takeExtension fp
                        in ext == ".yaml" || ext == ".yml"


listFilesInDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
listFilesInDir dir f = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      relativePaths <- listDirectory dir
      let absPaths = map (dir </>) relativePaths
      files <- filterM doesFileExist absPaths
      return $ filter f files

appendToFile :: String -> FilePath -> IO ()
appendToFile line file = do
  exists <- doesFileExist file
  if exists
    then appendFile file (line ++ "\n")
    else do
      withFile file WriteMode (\_ -> return ())
      appendFile file (line ++ "\n")
