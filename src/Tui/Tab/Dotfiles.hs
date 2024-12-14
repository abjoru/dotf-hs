module Tui.Tab.Dotfiles (dotfileTab) where

import           Brick                (Widget, hLimitPercent, str, vLimit,
                                       withAttr, (<+>), (<=>))
import           Brick.Widgets.Border (border, borderWithLabel)
import           Brick.Widgets.Center (hCenter)
import           Brick.Widgets.Edit   (renderEditor)
import qualified Brick.Widgets.List   as L
import           Dotf.Types           (TrackedType)
import           Lens.Micro           ((^.))
import           Tui.State            (Focus (FTracked, FUntracked), RName,
                                       State (_commit, _filter, _ignore, _tracked, _untracked),
                                       Tab (DotfileTab), commitEditL, commitL,
                                       countTracked, countUntracked,
                                       filterEditL, filterL, hasFocus,
                                       ignoreEditL, ignoreL)
import           Tui.Tab.Utils        (ToWidget (toWidget), filterTitle,
                                       tabLine, tip, title, ver)
import           Tui.Theme            (attrTitle)

dotfHelpLine1 :: [(String, String)]
dotfHelpLine1 = [ ("j/k:", " Up/Down ")
                , ("Ctrl+h/l:", " Switch Left/Right ")
                , ("(Shift) Tab:", " Switch Focus ")
                , ("f/F:", " Filter/Clear ")
                , ("q:", " Quit")
                ]

dotfHelpLine2 :: [(String, String)]
dotfHelpLine2 = [ ("e:", " Edit ")
                , ("d:", " Diff ")
                , ("a:", " Show All ")
                , ("c:", " Commit ")
                , ("A/R:", " Add/Remove File ")
                , ("I:", " Ignore Untracked")
                ]

dotfileTab :: State -> Widget RName
dotfileTab s = drawUi (_ignore s) (_filter s) (_commit s)
 where
  drawUi True _ _    = tabComp <=> (trackedComp <+> untrackedComp) <=> ignoreComp s <=> dotfHelp
  drawUi _ True _    = tabComp <=> (trackedComp <+> untrackedComp) <=> filterComp s <=> dotfHelp
  drawUi _ _ True    = tabComp <=> (trackedComp <+> untrackedComp) <=> commitComp s <=> dotfHelp
  drawUi _ _ _       = tabComp <=> (trackedComp <+> untrackedComp) <=> dotfHelp
  tabComp        = tabLine DotfileTab ver
  tcount         = countTracked s
  ucount         = countUntracked s
  trackedTitle   = title (filterTitle s ++ " Tracked " ++ show tcount ++ " ") $ hasFocus FTracked s
  untrackedTitle = title (filterTitle s ++ " Untracked " ++ show ucount ++ " ") $ hasFocus FUntracked s
  mkTracked      = trackedList (_tracked s) $ hasFocus FTracked s
  mkUntracked    = untrackedList (_untracked s) $ hasFocus FUntracked s
  trackedComp    = borderWithLabel trackedTitle mkTracked
  untrackedComp  = borderWithLabel untrackedTitle mkUntracked

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
dotfHelp =
  let line1 = foldr ((<+>) . tip) (str "") dotfHelpLine1
      line2 = foldr ((<+>) . tip) (str "") dotfHelpLine2
  in border . hCenter $ hCenter line1 <=> hCenter line2
