module Tui.Theme (
  theme,
  attrKey,
  attrBold,
  attrUnder,
  attrTitle,
  attrTitleFocus,
  attrTab,
  attrTabFocus
) where

import           Brick                (AttrName, attrName, fg, on)
import           Brick.Themes         (Theme, newTheme)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L
import           Graphics.Vty         (black, bold, brightBlack, brightMagenta,
                                       brightWhite, brightYellow, underline,
                                       white, withStyle, yellow)

theme :: Theme
theme = newTheme
  (white `on` brightBlack)
  [ (L.listAttr, fg brightWhite)
  , (L.listSelectedAttr, fg brightWhite)
  , (L.listSelectedFocusedAttr, black `on` brightYellow)
  , (D.dialogAttr, fg brightWhite)
  , (D.buttonAttr, brightBlack `on` white)
  , (D.buttonSelectedAttr, black `on` brightMagenta)
  , (B.borderAttr, fg white)
  , (E.editFocusedAttr, fg brightWhite)
  , (attrKey, withStyle (fg brightMagenta) bold)
  , (attrBold, withStyle (fg white) bold)
  , (attrUnder, withStyle (fg brightWhite) underline)
  , (attrTitle, withStyle (fg brightWhite) bold)
  , (attrTitleFocus, withStyle (fg yellow) bold)
  , (attrTab, fg brightWhite)
  , (attrTabFocus, withStyle (fg white) bold)
  ]

attrKey :: AttrName
attrKey = attrName "key"

attrBold :: AttrName
attrBold = attrName "bold"

attrUnder :: AttrName
attrUnder = attrName "under"

attrTitle :: AttrName
attrTitle = attrName "title"

attrTitleFocus :: AttrName
attrTitleFocus = attrName "title-focus"

attrTab :: AttrName
attrTab = attrName "tab"

attrTabFocus :: AttrName
attrTabFocus = attrName "tab-focus"
