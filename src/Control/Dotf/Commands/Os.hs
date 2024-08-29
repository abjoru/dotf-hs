module Control.Dotf.Commands.Os (
  editFile,
  installParu,
) where

import System.Process (callProcess)

editFile :: FilePath -> IO ()
editFile fp = callProcess "nvim" [fp]

installParu :: IO ()
installParu = pure ()
