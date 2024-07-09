module Tui.Widgets (
  ui,
  dotfileTab,
  bundleTab
) where

import           Brick                      (Padding (Max), Widget, emptyWidget,
                                             padLeft, str, vBox, vLimit,
                                             withAttr, withBorderStyle, (<=>))
import           Brick.Widgets.Border       (border)
import           Brick.Widgets.Border.Style (unicodeBold)
import           Brick.Widgets.Center       (hCenter, vCenter)

import           Tui.Popup                  (renderPopup)
import           Tui.State                  (RName, State (..), Tab (..))
import           Tui.Theme                  (attrTab, attrTabFocus)

ui :: State -> [Widget RName]
ui st = maybe [drawUi] (\p -> [renderPopup p, drawUi]) (_popup st)
  where drawUi = case _tabSel st of
                   DotfileTab -> dotfileTab st
                   BundleTab  -> bundleTab st

dotfileTab :: State -> Widget RName
dotfileTab _ = emptyWidget

bundleTab :: State -> Widget RName
bundleTab _ = emptyWidget

title :: String -> Widget RName
title t = withBorderStyle unicodeBold . border . vLimit 1 . vCenter . hCenter . vBox $ [str t]

tabSelector :: [Tab] -> Tab -> Widget RName
tabSelector tabs selected = vBox $ map renderTab tabs
  where renderTab t | selected == t = withAttr attrTabFocus $ str $ show t
                    | otherwise     = withAttr attrTab $ str $ show t

tabLine :: [Tab] -> Tab -> String -> Widget RName
tabLine ts t n = title n <=> padLeft Max (tabSelector ts t)
