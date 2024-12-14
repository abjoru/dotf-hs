module Tui.Event.XMonad (xmonadEvent) where

import           Brick               (zoom)
import           Brick.Widgets.List  (handleListEvent, handleListEventVi)
import           Control.Monad.State (MonadIO (liftIO))
import           Dotf.Utils          (resolveAppConfig)
import           Graphics.Vty
import           Tui.State

--------------------
-- Event Handlers --
--------------------

xmonadEvent :: Focus -> Event -> DEvent ()
xmonadEvent _ (EvKey (KChar 'e') []) = doEditApps
xmonadEvent FAppFavoritesList ev     = favoritesListEvent ev
xmonadEvent FAppGamesList ev         = gamesListEvent ev
xmonadEvent FAppInternetList ev      = internetListEvent ev
xmonadEvent FAppSettingsList ev      = settingsListEvent ev
xmonadEvent FAppSystemList ev        = systemListEvent ev
xmonadEvent FAppOfficeList ev        = officeListEvent ev
xmonadEvent _ _                      = return ()

favoritesListEvent :: Event -> DEvent ()
favoritesListEvent ev = zoom appFavoritesL $ handleListEventVi handleListEvent ev

gamesListEvent :: Event -> DEvent ()
gamesListEvent ev = zoom appGamesL $ handleListEventVi handleListEvent ev

internetListEvent :: Event -> DEvent ()
internetListEvent ev = zoom appInternetL $ handleListEventVi handleListEvent ev

settingsListEvent :: Event -> DEvent ()
settingsListEvent ev = zoom appSettingsL $ handleListEventVi handleListEvent ev

systemListEvent :: Event -> DEvent ()
systemListEvent ev = zoom appSystemL $ handleListEventVi handleListEvent ev

officeListEvent :: Event -> DEvent ()
officeListEvent ev = zoom appOfficeL $ handleListEventVi handleListEvent ev

-------------
-- Actions --
-------------

doEditApps :: DEvent ()
doEditApps = do
  file <- liftIO resolveAppConfig
  maybeEditFile (Just file) >> syncApps
