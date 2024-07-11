module Data.Dotf (
  module Data.Dotf.Bundles,
  module Data.Dotf.Os,

  TrackedType(..),
  GitError(..),

  ErrorOrString,
  ErrorOrTracked,
  ErrorOrFilePaths,

  Answer,

  trackedFile
) where

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C8

import           Data.Char                  (isLetter, toLower)
import           Data.Dotf.Bundles
import           Data.Dotf.Os

import           System.IO                  (hFlush, stdout)

data TrackedType
  = Tracked FilePath
  | Staged FilePath
  | Unstaged FilePath
  deriving (Show, Eq)

data GitError = GitError {
  errorCode    :: Int,
  errorMessage :: B.ByteString
} deriving Eq

instance Show GitError where
  show err = concat ["Error occurred: ", show (errorCode err), " - ", C8.unpack (errorMessage err)]

type ErrorOrString = Either GitError String
type ErrorOrFilePaths = Either GitError [FilePath]
type ErrorOrTracked = Either GitError [TrackedType]

data Answer = Yes | No | Quit
  deriving Show

instance Read Answer where
  readsPrec _ input = case fmap toLower . filter isLetter $ input of
    "quit" -> [(Quit, [])]
    "q"    -> [(Quit, [])]
    "yes"  -> [(Yes, [])]
    "y"    -> [(Yes, [])]
    "no"   -> [(No, [])]
    "n"    -> [(No, [])]
    _      -> [(No, [])]

trackedFile :: TrackedType -> FilePath
trackedFile (Tracked fp)  = fp
trackedFile (Staged fp)   = fp
trackedFile (Unstaged fp) = fp

-- Example: Move to commands in separate func!
askNewBareRepo :: FilePath -> IO Answer
askNewBareRepo path = do
  putStr ("Do you really want to create a new bare repo at '" ++ path ++ "'? (q/y/N) ")
  hFlush stdout
  read <$> getLine
