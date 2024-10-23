{-# LANGUAGE FlexibleInstances #-}
module Tui.Widgets (
  ui,
  dotfileTab,
  bundleTab,
) where

import           Paths_dotf_hs        (version)

import           Brick                (Padding (Max), Widget, hLimitPercent,
                                       padLeft, str, vLimit, withAttr, (<+>),
                                       (<=>))
import           Brick.Widgets.Border (border, borderWithLabel)
import qualified Brick.Widgets.List   as L

import           Brick.Widgets.Center (hCenter)
import           Brick.Widgets.Core   (Padding (Pad), hBox, padRight)
import           Brick.Widgets.Edit   (getEditContents, renderEditor)
import qualified Brick.Widgets.Table  as T
import qualified Data.Vector          as V
import           Data.Version         (showVersion)
import           Dotf.Types
import           Lens.Micro
import           Tui.Popup            (renderPopup)
import           Tui.State
import           Tui.Theme

class ToWidget a where
  -- Uses componentFocus -> elementFocus -> element
  toWidget :: Bool -> Bool -> a -> Widget RName

instance ToWidget TrackedType where
  toWidget cFocus focus t = withAttr (attr t (cFocus && focus)) $ str (show t)

instance ToWidget FilePath where
  toWidget cFocus focus a = withAttr (attr a (cFocus && focus)) $ str a

instance ToWidget Tab where
  toWidget _ eFocus t = withAttr (attr t eFocus) $ padRight (Pad 1) $ str (show t)

instance ToWidget Bundle where
  toWidget _ eFocus b = 
    if bundleHeadless b
    then withAttr (attr b eFocus) $ str (bundleName b ++ " [Headless]")
    else withAttr (attr b eFocus) $ str (bundleName b)

ver :: String
ver = "DotF " ++ showVersion version

pkgHeaderRow :: PkgRow
pkgHeaderRow = PkgRow "Name" "Arch" "OSX" "Deb"

gitHeaderRow :: GitRow
gitHeaderRow = GitRow "Name" "URL" "Branch"

pkgColAlign :: [T.ColumnAlignment]
pkgColAlign = [T.AlignLeft, T.AlignLeft, T.AlignLeft, T.AlignLeft]

pkgColWidths :: [PkgRow] -> [Int]
pkgColWidths rows =
  [ maximum . map (\(PkgRow n _ _ _) -> length n + 2) $ pkgHeaderRow : rows
  , maximum . map (\(PkgRow _ a _ _) -> length a + 2) $ pkgHeaderRow : rows
  , maximum . map (\(PkgRow _ _ b _) -> length b + 2) $ pkgHeaderRow : rows
  , maximum . map (\(PkgRow _ _ _ c) -> length c + 2) $ pkgHeaderRow : rows
  ]

gitColAlign :: [T.ColumnAlignment]
gitColAlign = [T.AlignLeft, T.AlignLeft, T.AlignLeft]

gitColWidths :: [GitRow] -> [Int]
gitColWidths rows =
  [ maximum . map (\(GitRow a _ _) -> length a + 2) $ gitHeaderRow : rows
  , maximum . map (\(GitRow _ b _) -> length b + 2) $ gitHeaderRow : rows
  , maximum . map (\(GitRow _ _ c) -> length c + 2) $ gitHeaderRow : rows
  ]

ui :: State -> [Widget RName]
ui st = maybe [drawUi] (\p -> [renderPopup p, drawUi]) (_popup st)
 where
  drawUi = case _tab st of
    DotfileTab -> dotfileTab st
    BundleTab  -> bundleTab st

dotfileTab :: State -> Widget RName
dotfileTab s = drawUi (_ignore s) (_filter s) (_commit s)
 where
  drawUi True _ _    = tabComp <=> (trackedComp <+> untrackedComp) <=> ignoreComp s <=> dotfHelp
  drawUi _ True _    = tabComp <=> (trackedComp <+> untrackedComp) <=> filterComp s <=> dotfHelp
  drawUi _ _ True    = tabComp <=> (trackedComp <+> untrackedComp) <=> commitComp s <=> dotfHelp
  drawUi _ _ _ = tabComp <=> (trackedComp <+> untrackedComp) <=> dotfHelp
  tabComp = tabLine DotfileTab ver
  tcount = countTracked s
  ucount = countUntracked s
  trackedTitle = title (filterTitle s ++ " Tracked " ++ show tcount ++ " ") $ hasFocus FTracked s
  untrackedTitle = title (filterTitle s ++ " Untracked " ++ show ucount ++ " ") $ hasFocus FUntracked s
  mkTracked = trackedList (_tracked s) $ hasFocus FTracked s
  mkUntracked = untrackedList (_untracked s) $ hasFocus FUntracked s
  trackedComp = borderWithLabel trackedTitle mkTracked
  untrackedComp = borderWithLabel untrackedTitle mkUntracked

filterTitle :: State -> String
filterTitle state =
  let editor = _filterEdit state
      content = getEditContents editor
  in case head content of
    "" -> ""
    _  -> "[Filter] "

trackedList :: L.List RName TrackedType -> Bool -> Widget RName
trackedList l focus = L.renderList (toWidget focus) focus l

untrackedList :: L.List RName FilePath -> Bool -> Widget RName
untrackedList l focus = L.renderList (toWidget focus) focus l

ignoreComp :: State -> Widget RName
ignoreComp state = border $ withAttr attrTitle $ str "Ignore: " <+> (hLimitPercent 100 . vLimit 1 $ editor)
 where editor = renderEditor (str . unlines) (state ^. ignoreL) (state ^. ignoreEditL)

filterComp :: State -> Widget RName
filterComp state = border $ withAttr attrTitle $ str "Filter (regex): " <+> (hLimitPercent 100 . vLimit 1 $ editor)
  where editor = renderEditor (str . unlines) (state ^. filterL) (state ^. filterEditL)

commitComp :: State -> Widget RName
commitComp state = border $ withAttr attrTitle $ str "Commit Msg: " <+> (hLimitPercent 100 . vLimit 1 $ editor)
  where editor = renderEditor (str . unlines) (state ^. commitL) (state ^. commitEditL)

dotfHelp :: Widget RName
dotfHelp = border . hCenter $
    hCenter (   tip "j/k:" " Up/Down  "
            <+> tip "Ctrl+h/l:" " Switch Left/Right  "
            <+> tip "(Shift) Tab:" " Switch Focus  "
            <+> tip "f/F" " Filter/Clear "
            <+> tip "q:" " Quit"
            ) <=>
    hCenter (   tip "e:" " Edit  "
            <+> tip "d:" " Diff "
            <+> tip "a:" " Show All  "
            <+> tip "c:" " Commit  "
            <+> tip "A/R:" " Add/Remove File  "
            <+> tip "I:" " Ignore Untracked  "
            )
 where tip k v = withAttr attrTitle $ str k <+> withAttr attrItem (str v)

bundleHelp :: Widget RName
bundleHelp = border . hCenter $
    hCenter (   tip "j/k:" " Up/Down  "
            <+> tip "Esc:" " Deselect  "
            <+> tip "(Shift) Tab:" " Switch Focus  "
            <+> tip "q:" " Quit"
            ) <=>
    hCenter (tip "n:" " New Bundle  " <+> tip "e:" " Edit")
 where tip k v = withAttr attrTitle $ str k <+> withAttr attrItem (str v)

bundleTab :: State -> Widget RName
bundleTab s = drawUi (_newBundle s)
 where
   drawUi False = tabLine BundleTab ver
                  <=> ((bc <=> sc) <+> (pc <=> gc))
                  <=> bundleHelp
   drawUi True = tabLine BundleTab ver
                 <=> ((bc <=> sc) <+> (pc <=> gc))
                 <=> newBundleComp s
                 <=> bundleHelp
   bc = bundleList s (hasFocus FBundleList s)
   sc = scriptList s (hasFocus FScriptList s)
   pc = packageList s (hasFocus FPackageList s)
   gc = gitPackageList s (hasFocus FGitPackageList s)

bundleList :: State -> Bool -> Widget RName
bundleList s focus = borderWithLabel mkTitle $ L.renderList (toWidget focus) focus (s ^. bundlesL)
 where mkTitle = title " Bundles " $ hasFocus FBundleList s

packageList :: State -> Bool -> Widget RName
packageList s focus =
  let rowList = s ^. packagesL
      rowWidths = pkgColWidths (V.toList $ L.listElements rowList)
      listTitle = title " Packages " $ hasFocus FPackageList s
      headers = hBox $ T.alignColumns pkgColAlign rowWidths $ rHead pkgHeaderRow
   in borderWithLabel listTitle $ headers <=> L.renderList (mkRow rowWidths) focus rowList
 where
   mkRow w _ r = hBox $ T.alignColumns pkgColAlign w (rRow r)
   rRow (PkgRow n a b c) = [str n, str a, str b, str c]
   rHead (PkgRow n a b c) = [ withAttr attrTitle $ str n
                            , withAttr attrTitle $ str a
                            , withAttr attrTitle $ str b
                            , withAttr attrTitle $ str c
                            ]

gitPackageList :: State -> Bool -> Widget RName
gitPackageList s focus =
  let rowList = s ^. gitPackagesL
      rowWidths = gitColWidths (V.toList $ L.listElements rowList)
      listTitle = title " Git Packages " $ hasFocus FGitPackageList s
      headers = hBox $ T.alignColumns gitColAlign rowWidths $ rHead gitHeaderRow
   in borderWithLabel listTitle $ headers <=> L.renderList (mkRow rowWidths) focus rowList
 where
   mkRow w _ r = hBox $ T.alignColumns gitColAlign w (rRow r)
   rRow (GitRow a b c) = [str a, str b, str c]
   rHead (GitRow a b c) = [ withAttr attrTitle $ str a
                          , withAttr attrTitle $ str b
                          , withAttr attrTitle $ str c
                          ]

scriptList :: State -> Bool -> Widget RName
scriptList s focus = borderWithLabel mkTitle $ L.renderList (toWidget focus) focus (s ^. scriptsL)
 where mkTitle = title " Scripts " $ hasFocus FScriptList s

newBundleComp :: State -> Widget RName
newBundleComp state = border $ withAttr attrTitle $ str "Name: " <+> (hLimitPercent 100 . vLimit 1 $ editor)
 where editor = renderEditor (str . unlines) (state ^. newBundleL) (state ^. newBundleEditL)

tabSelector :: Tab -> Widget RName
tabSelector selected = padLeft (Pad 1) . hBox $ map render [DotfileTab, BundleTab]
 where render t = toWidget True (selected == t) t

tabLine :: Tab -> String -> Widget RName
tabLine t n = tabSelector t <+> (padLeft Max . padRight (Pad 1) $ withAttr attrAppName (str n))

title :: String -> Bool -> Widget RName
title txt True  = withAttr attrTitleFocus $ str txt
title txt False = withAttr attrTitle $ str txt
