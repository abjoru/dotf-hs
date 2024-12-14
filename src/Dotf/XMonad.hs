module Dotf.XMonad (
  loadAppConfig,
  loadAppConfig',
  collectAppLaunchers
) where

import           Control.Monad.Extra (ifM)
import qualified Data.Yaml           as Y
import           Dotf.Types
import           Dotf.Utils
import           System.Directory    (doesFileExist)

loadAppConfig :: IO (Either Y.ParseException AppConfig)
loadAppConfig = resolveAppConfig >>= loadAppConfig'

loadAppConfig' :: FilePath -> IO (Either Y.ParseException AppConfig)
loadAppConfig' f = ifM (doesFileExist f) (Y.decodeFileEither f) $ pure $ Right emptyAppConfig

collectAppLaunchers :: AppCategory -> AppConfig -> [AppLauncher]
collectAppLaunchers CFavorites = _favorites
collectAppLaunchers CGames     = _games
collectAppLaunchers CInternet  = _internet
collectAppLaunchers CSettings  = _settings
collectAppLaunchers CSystem    = _system
collectAppLaunchers COffice    = _office
