module Tui.Theme (
  theme,
  attrKey,
  attrBold,
  attrUnder,
  attrTitle,
  attrTitleFocus,
  attrTab,
  attrTabFocus,
  attrAppName,
  attrSelItem,
  attrSelUnfocusItem,
  attrItem,
  attrStagedItem,
  attrUnstagedItem
) where

import           Brick                (AttrName, attrName, fg, on)
import           Brick.Themes         (Theme, newTheme)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L
import           Graphics.Vty         (Color, black, bold, brightBlack,
                                       brightCyan, brightMagenta, brightWhite,
                                       brightYellow, green, red, rgbColor,
                                       underline, white, withStyle, yellow)

absBlack :: Color
absBlack = rgbColor (0 :: Integer) 0 0

theme :: Theme
theme = newTheme
  (white `on` absBlack)
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
  , (attrAppName, withStyle (fg brightCyan) bold)
  , (attrSelItem, black `on` yellow)
  , (attrItem, fg brightWhite)
  , (attrStagedItem, fg green)
  , (attrUnstagedItem, fg red)
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

attrAppName :: AttrName
attrAppName = attrName "app-name"

attrSelItem :: AttrName
attrSelItem = attrName "selected-item"

attrSelUnfocusItem :: AttrName
attrSelUnfocusItem = attrName "selected-item-unfocused"

attrItem :: AttrName
attrItem = attrName "item"

attrStagedItem :: AttrName
attrStagedItem = attrName "staged-item"

attrUnstagedItem :: AttrName
attrUnstagedItem = attrName "unstaged-item"
