module AppConfigSpec (spec) where

import           Test.Hspec

import           Control.Arrow (right)
import           Data.Either   (fromRight, isRight)
import           Dotf.Types
import           Dotf.XMonad   (loadAppConfig')

spec :: Spec
spec = do
  describe "Manage application launchers" $ do
    it "parses XMonad application.yaml file" $ do
      result <- loadAppConfig' "test/applications.yaml"
      result `shouldSatisfy` isRight

      -- Check that we have launchers for each category
      fromRight [] (right _favorites result) `shouldNotBe` []
      fromRight [] (right _games result) `shouldNotBe` []
      fromRight [] (right _internet result) `shouldNotBe` []
      fromRight [] (right _settings result) `shouldNotBe` []
      fromRight [] (right _system result) `shouldNotBe` []
      fromRight [] (right _office result) `shouldNotBe` []

