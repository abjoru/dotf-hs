module Tui.Widgets (
  ui,
  dotfileTab,
  bundleTab
) where

import           Brick                (Padding (Max), Widget, emptyWidget,
                                       padLeft, str, withAttr, (<+>), (<=>))
import           Brick.Widgets.Border (border, borderWithLabel)
import qualified Brick.Widgets.List   as L

import           Data.Dotf            (TrackedType (..), trackedFile)

import           Brick.Widgets.Center (hCenter)
import           Brick.Widgets.Core   (Padding (Pad), hBox, padRight)
import           Tui.Popup            (renderPopup)
import           Tui.State            (Focus (..), RName, State (..), Tab (..),
                                       countTracked, countUntracked, hasFocus)
import           Tui.Theme            (attrAppName, attrItem, attrSelItem,
                                       attrStagedItem, attrTab, attrTabFocus,
                                       attrTitle, attrTitleFocus,
                                       attrUnstagedItem)

ui :: State -> [Widget RName]
ui st = maybe [drawUi] (\p -> [renderPopup p, drawUi]) (_popup st)
  where drawUi = case _tab st of
                   DotfileTab -> dotfileTab st
                   BundleTab  -> bundleTab st

dotfileTab :: State -> Widget RName
dotfileTab s = tabComp <=> (trackedComp <+> untrackedComp) <=> dotfHelp
  where tabComp        = tabLine DotfileTab "DotF 1.0"
        tcount         = countTracked s
        ucount         = countUntracked s
        trackedTitle   = title (" Tracked " ++ show tcount ++ " ") $ hasFocus FTracked s
        untrackedTitle = title (" Untracked " ++ show ucount ++ " ") $ hasFocus FUntracked s
        mkTracked      = trackedList (_tracked s) $ hasFocus FTracked s
        mkUntracked    = untrackedList (_untracked s) $ hasFocus FUntracked s
        trackedComp    = borderWithLabel trackedTitle mkTracked
        untrackedComp  = borderWithLabel untrackedTitle mkUntracked

trackedList :: L.List RName TrackedType -> Bool -> Widget RName
trackedList l focus = L.renderList (drawItem focus) focus l
  where drawItem True True t       = withAttr attrSelItem $ str (trackedFile t)
        drawItem _ _ (Tracked fp)  = withAttr attrItem $ str fp
        drawItem _ _ (Staged fp)   = withAttr attrStagedItem $ str fp
        drawItem _ _ (Unstaged fp) = withAttr attrUnstagedItem $ str fp

untrackedList :: L.List RName FilePath -> Bool -> Widget RName
untrackedList l focus = L.renderList (drawItem focus) focus l
  where drawItem True True fp = withAttr attrSelItem $ str fp
        drawItem _ _ fp       = withAttr attrItem $ str fp

dotfHelp :: Widget RName
dotfHelp = border . hCenter $
         hCenter (tip "j/k:" " Up/Down  "
         <+> tip "Ctrl+h/l:" " Switch Left/Right  "
         <+> tip "Tab:" " Switch Focus  "
         <+> tip "q:" " Quit") <=>
         hCenter (tip "e:" " Edit  "
         <+> tip "a:" " Show All  "
         <+> tip "c:" " Commit  "
         <+> tip "A/R:" " Add/Remove File  "
         <+> tip "I:" " Ignore Untracked  ")
  where tip k v = withAttr attrTitle $ str k <+> withAttr attrItem (str v)

bundleTab :: State -> Widget RName
bundleTab s = tabComp <=> ((blComp <=> scComp) <+> (pkComp <=> gtComp)) <=> dotfHelp
  where tabComp = tabLine BundleTab "DotF 1.0"
        blComp = borderWithLabel (str " Bundles ") $ str "my bundle list"
        scComp = borderWithLabel (str " Scripts ") $ str "my script list"
        pkComp = borderWithLabel (str " Packages ") $ str "my package list"
        gtComp = borderWithLabel (str " Git Packages ") $ str "my git package list"

tabSelector :: Tab -> Widget RName
tabSelector selected = padLeft (Pad 1) . hBox $ map renderTab [DotfileTab, BundleTab]
  where renderTab t | selected == t = withAttr attrTabFocus $ padRight (Pad 1) $ str $ show t
                    | otherwise     = withAttr attrTab $ padRight (Pad 1) $ str $ show t

tabLine :: Tab -> String -> Widget RName
tabLine t n = tabSelector t <+> (padLeft Max . padRight (Pad 1) $ withAttr attrAppName (str n))

title :: String -> Bool -> Widget RName
title txt True  = withAttr attrTitleFocus $ str txt
title txt False = withAttr attrTitle $ str txt
