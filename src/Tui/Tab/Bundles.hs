module Tui.Tab.Bundles (bundleTab) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Edit   (renderEditor)
import qualified Brick.Widgets.List   as L
import qualified Brick.Widgets.Table  as T
import qualified Data.Vector          as V
import           Lens.Micro           ((^.))
import           Tui.State
import           Tui.Tab.Utils
import           Tui.Theme

bundleHelpLine1 :: [(String, String)]
bundleHelpLine1 = [ ("j/k:", " Up/Down ")
                  , ("Esc:", " Deselect ")
                  , ("(Shift) Tab:", " Switch Focus ")
                  , ("q:", " Quit")
                  ]

bundleHelpLine2 :: [(String, String)]
bundleHelpLine2 = [ ("n:", " New Bundle "), ("e:", " Edit")]

pkgHeaderRow :: PkgRow
pkgHeaderRow = PkgRow "Name" "Arch" "OSX" "Cask" "Deb"

gitHeaderRow :: GitRow
gitHeaderRow = GitRow "Name" "URL" "Branch"

pkgColWidths :: [PkgRow] -> [Int]
pkgColWidths rows =
  [ maximum . map (\(PkgRow n _ _ _ _) -> length n + 2) $ pkgHeaderRow : rows
  , maximum . map (\(PkgRow _ a _ _ _) -> length a + 2) $ pkgHeaderRow : rows
  , maximum . map (\(PkgRow _ _ b _ _) -> length b + 2) $ pkgHeaderRow : rows
  , maximum . map (\(PkgRow _ _ _ c _) -> length c + 2) $ pkgHeaderRow : rows
  , maximum . map (\(PkgRow _ _ _ _ d) -> length d + 2) $ pkgHeaderRow : rows
  ]

gitColAlign :: [T.ColumnAlignment]
gitColAlign = [T.AlignLeft, T.AlignLeft, T.AlignLeft]

gitColWidths :: [GitRow] -> [Int]
gitColWidths rows =
  [ maximum . map (\(GitRow a _ _) -> length a + 2) $ gitHeaderRow : rows
  , maximum . map (\(GitRow _ b _) -> length b + 2) $ gitHeaderRow : rows
  , maximum . map (\(GitRow _ _ c) -> length c + 2) $ gitHeaderRow : rows
  ]

bundleHelp :: Widget RName
bundleHelp =
  let line1 = foldr ((<+>) . tip) (str "") bundleHelpLine1
      line2 = foldr ((<+>) . tip) (str "") bundleHelpLine2
  in border . hCenter $ hCenter line1 <=> hCenter line2

bundleTab :: State -> Widget RName
bundleTab s = drawUi (_newBundle s)
 where drawUi foc = top <=> middle <=> bottom foc
       top = tabLine BundleTab ver
       middle = left <+> right
       bottom True  = newBundleComp s <=> bundleHelp
       bottom False = bundleHelp
       left = bc <=> sc <=> gc
       right = pc
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
   rRow (PkgRow n a b c d) = [str n, str a, str b, str c, str d]
   rHead (PkgRow n a b c d) = [ withAttr attrTitle $ str n
                              , withAttr attrTitle $ str a
                              , withAttr attrTitle $ str b
                              , withAttr attrTitle $ str c
                              , withAttr attrTitle $ str d
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
