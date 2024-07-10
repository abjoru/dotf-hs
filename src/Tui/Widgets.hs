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
                                       hasFocus)
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
        trackedTitle   = title " Tracked " $ hasFocus FTracked s
        untrackedTitle = title " Untracked " $ hasFocus FUntracked s
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
         ( hCenter $ (tip "j/k:" " up/down  ")
         <+> (tip "Ctrl+h/l:" " switch left/right  ")
         <+> (tip "Tab:" " switch focus  ")
         <+> (tip "q:" " quit")
         ) <=>
         ( hCenter $ (tip "e:" " edit  ")
         <+> (tip "a:" " add modified  ")
         <+> (tip "c:" " commit  ")
         <+> (tip "I:" " ignore untracked  ")
         <+> (tip "T:" " track file  ")
         <+> (tip "U:" " untrack file")
         )
  where tip k v = withAttr attrTitle $ str k <+> (withAttr attrItem $ str v)

bundleTab :: State -> Widget RName
bundleTab _ = emptyWidget

tabSelector :: Tab -> Widget RName
tabSelector selected = padLeft (Pad 1) . hBox $ map renderTab [DotfileTab, BundleTab]
  where renderTab t | selected == t = withAttr attrTabFocus $ padRight (Pad 1) $ str $ show t
                    | otherwise     = withAttr attrTab $ padRight (Pad 1) $ str $ show t

tabLine :: Tab -> String -> Widget RName
tabLine t n = tabSelector t <+> (padLeft Max . padRight (Pad 1) $ withAttr attrAppName (str n))

title :: String -> Bool -> Widget RName
title txt True  = withAttr attrTitleFocus $ str txt
title txt False = withAttr attrTitle $ str txt
