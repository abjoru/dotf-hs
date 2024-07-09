module Tui.Widgets (
  ui,
  dotfileTab,
  bundleTab
) where

import           Brick                (Padding (Max), Widget, emptyWidget,
                                       padLeft, str, vBox, withAttr, (<+>),
                                       (<=>))
import           Brick.Widgets.Border (borderWithLabel)
import           Brick.Widgets.List   as L

import           Data.Dotf            (TrackedType (Tracked))

import           Tui.Popup            (renderPopup)
import qualified Tui.State            as S
import           Tui.State            (RName, State (..), Tab (..), hasFocus)
import           Tui.Theme            (attrAppName, attrTab, attrTabFocus,
                                       attrTitle, attrTitleFocus)

ui :: State -> [Widget RName]
ui st = maybe [drawUi] (\p -> [renderPopup p, drawUi]) (_popup st)
  where drawUi = case _tabSel st of
                   DotfileTab -> dotfileTab st
                   BundleTab  -> bundleTab st

dotfileTab :: State -> Widget RName
dotfileTab s = tabComp <+> (trackedComp <=> untrackedComp)
  where tabComp        = tabLine DotfileTab "DotF 1.0"
        trackedTitle   = title "Tracked" $ hasFocus S.Tracked s
        untrackedTitle = title "Untracked" $ hasFocus S.Untracked s
        trackedComp    = borderWithLabel trackedTitle (trackedList $ _tracked s)
        untrackedComp  = borderWithLabel untrackedTitle (untrackedList $ _untracked s)

trackedList :: L.List RName TrackedType -> Widget RName
trackedList _ = emptyWidget

untrackedList :: L.List RName FilePath -> Widget RName
untrackedList _ = emptyWidget

bundleTab :: State -> Widget RName
bundleTab _ = emptyWidget

tabSelector :: Tab -> Widget RName
tabSelector selected = vBox $ map renderTab [DotfileTab, BundleTab]
  where renderTab t | selected == t = withAttr attrTabFocus $ str $ show t
                    | otherwise     = withAttr attrTab $ str $ show t

tabLine :: Tab -> String -> Widget RName
tabLine t n = tabSelector t <=> padLeft Max (withAttr attrAppName $ str n)

title :: String -> Bool -> Widget RName
title txt True  = withAttr attrTitleFocus $ str txt
title txt False = withAttr attrTitle $ str txt
