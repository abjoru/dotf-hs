module BundlesSpec (spec) where

import           Test.Hspec

import           Data.Either
import           Dotf.Bundles

bundleFiles :: [FilePath]
bundleFiles = ["test/bundles/shell.yaml", "test/bundles/desktop.yaml"]

spec :: Spec
spec = do
  describe "Manage application bundles" $ do
    it "parses bundle yaml files" $ do
      result <- loadBundles' bundleFiles
      result `shouldSatisfy` isRight
