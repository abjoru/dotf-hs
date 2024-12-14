module Tui.Tab.XMonad (xmonadTab) where

import           Brick                (Widget, hBox, str, withAttr, (<+>),
                                       (<=>))
import           Brick.Widgets.Border (border, borderWithLabel)
import           Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.List   as L
import qualified Brick.Widgets.Table  as T
import qualified Data.Vector          as V
import           Dotf.Types           (AppCategory (..))
import           Lens.Micro           ((^.))
import           Tui.State            (AppRow (..), Focus (..), RName, State,
                                       Tab (XMonadTab), appFavoritesL,
                                       appGamesL, appInternetL, appOfficeL,
                                       appSettingsL, appSystemL, hasFocus)
import           Tui.Tab.Utils        (pkgColAlign, tabLine, tip, title, ver)
import           Tui.Theme            (attrTitle)

appHeaderRow :: AppRow
appHeaderRow = AppRow "Name" "Description"

xmonadHelpLine1 :: [(String, String)]
xmonadHelpLine1 = [ ("j/k:", " Up/Down ")
                  , ("Esc:", " Deselect ")
                  , ("(Shift) Tab:", " Switch Focus ")
                  , ("q:", " Quit")
                  ]

xmonadHelpLine2 :: [(String, String)]
xmonadHelpLine2 = [("e:", " Edit Applications ")]

appColWidths :: [AppRow] -> [Int]
appColWidths rows =
  [ maximum . map (\(AppRow n _) -> length n + 2) $ appHeaderRow : rows
  , maximum . map (\(AppRow _ d) -> length d + 2) $ appHeaderRow : rows
  ]

xmonadHelp :: Widget RName
xmonadHelp =
  let line1 = foldr ((<+>) . tip) (str "") xmonadHelpLine1
      line2 = foldr ((<+>) . tip) (str "") xmonadHelpLine2
  in border . hCenter $ hCenter line1 <=> hCenter line2

xmonadTab :: State -> Widget RName
xmonadTab s = top <=> middle <=> bottom
  where top          = tabLine XMonadTab ver
        middle       = right <+> left
        bottom       = xmonadHelp
        right        = favsList <=> internetList <=> sysList
        left         = gamesList <=> settingsList <=> officeList
        favsList     = appList s (hasFocus FAppFavoritesList s) CFavorites " Favorites "
        gamesList    = appList s (hasFocus FAppGamesList s) CGames " Games "
        internetList = appList s (hasFocus FAppInternetList s) CInternet " Internet "
        settingsList = appList s (hasFocus FAppSettingsList s) CSettings " Settings "
        sysList      = appList s (hasFocus FAppSystemList s) CSystem " System "
        officeList   = appList s (hasFocus FAppOfficeList s) COffice " Office "

appList :: State -> Bool -> AppCategory -> String -> Widget RName
appList s focus cat t =
  let rowList = case cat of
                  CFavorites -> s ^. appFavoritesL
                  CGames     -> s ^. appGamesL
                  CInternet  -> s ^. appInternetL
                  CSettings  -> s ^. appSettingsL
                  CSystem    -> s ^. appSystemL
                  COffice    -> s ^. appOfficeL
      rowWidths = appColWidths (V.toList $ L.listElements rowList)
      listTitle = title t $ hasFocus FAppFavoritesList s
      headers = hBox $ T.alignColumns pkgColAlign rowWidths $ rHead appHeaderRow
  in borderWithLabel listTitle $ headers <=> L.renderList (mkRow rowWidths) focus rowList
  where mkRow w _ r = hBox $ T.alignColumns pkgColAlign w (rRow r)
        rRow (AppRow n d) = [str n, str d]
        rHead (AppRow n d) = [withAttr attrTitle $ str n, withAttr attrTitle $ str d]

