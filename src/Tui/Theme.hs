{-# LANGUAGE FlexibleInstances #-}
module Tui.Theme (
  TypeAttr(..),

  theme,
  attrKey,
  attrBold,
  attrUnder,
  attrItem,
  attrTitle,
  attrTitleFocus,
  attrAppName,
) where

import           Brick                (AttrName, attrName, fg, on)
import           Brick.Themes         (Theme, newTheme)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit   as E
import qualified Brick.Widgets.List   as L
import           Dotf.Types           (Bundle, TrackedType (..))
import           Graphics.Vty         (Color, black, bold, brightBlack,
                                       brightCyan, brightMagenta, brightWhite,
                                       brightYellow, green, red, rgbColor,
                                       underline, white, withStyle, yellow)
import           Tui.State            (Tab)

class TypeAttr a where
  attr :: a -> Bool -> AttrName

instance TypeAttr TrackedType where
  attr (Tracked _) False    = attrItem
  attr (Staged _ _) False   = attrStagedItem
  attr (Staged _ _) True    = attrStagedSelItem
  attr (Unstaged _ _) False = attrUnstagedItem
  attr (Unstaged _ _) True  = attrUnstagedSelItem
  attr _ True               = attrSelItem

instance TypeAttr FilePath where
  attr _ True  = attrSelItem
  attr _ False = attrItem

instance TypeAttr Tab where
  attr _ True  = attrTabFocus
  attr _ False = attrTab

instance TypeAttr Bundle where
  attr _ True  = attrSelItem
  attr _ False = attrItem

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
  , (attrStagedSelItem, green `on` yellow)
  , (attrUnstagedItem, fg red)
  , (attrUnstagedSelItem, red `on` yellow)
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

attrItem :: AttrName
attrItem = attrName "item"

attrStagedItem :: AttrName
attrStagedItem = attrName "staged-item"

attrStagedSelItem :: AttrName
attrStagedSelItem = attrName "staged-sel-item"

attrUnstagedItem :: AttrName
attrUnstagedItem = attrName "unstaged-item"

attrUnstagedSelItem :: AttrName
attrUnstagedSelItem = attrName "unstaged-sel-item"
