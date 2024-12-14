{-# LANGUAGE FlexibleInstances #-}
module Tui.Tab.Utils where

import           Brick               (Padding (Max, Pad), Widget, hBox, padLeft,
                                      padRight, str, withAttr, (<+>))
import           Brick.Widgets.Edit  (getEditContents)
import qualified Brick.Widgets.Table as T
import           Data.Version        (showVersion)
import           Dotf.Types
import           Paths_dotf_hs       (version)
import           Tui.State
import           Tui.Theme

-----------------
-- Typeclasses --
-----------------

class ToWidget a where
  -- Uses componentFocus -> elementFocus -> element
  toWidget :: Bool -> Bool -> a -> Widget RName

---------------
-- Instances --
---------------

instance ToWidget TrackedType where
  toWidget cFocus focus t = withAttr (attr t (cFocus && focus)) $ str (show t)

instance ToWidget FilePath where
  toWidget cFocus focus a = withAttr (attr a (cFocus && focus)) $ str a

instance ToWidget Tab where
  toWidget _ eFocus t = withAttr (attr t eFocus) $ padRight (Pad 1) $ str (show t)

instance ToWidget Bundle where
  toWidget _ eFocus b
    | bundleHeadless b = withAttr (attr b eFocus) $ str (bundleName b ++ " [Headless]")
    | otherwise        = withAttr (attr b eFocus) $ str (bundleName b)

-------------
-- Widgets --
-------------

ver :: String
ver = "DotF " ++ showVersion version

tip :: (String, String) -> Widget n
tip (k, v) = withAttr attrTitle (str k) <+> withAttr attrItem (str v)

title :: String -> Bool -> Widget RName
title txt True  = withAttr attrTitleFocus $ str txt
title txt False = withAttr attrTitle $ str txt

filterTitle :: State -> String
filterTitle state =
  let editor = _filterEdit state
      content = getEditContents editor
  in case head content of
    "" -> ""
    _  -> "[Filter] "

pkgColAlign :: [T.ColumnAlignment]
pkgColAlign = [T.AlignLeft, T.AlignLeft, T.AlignLeft, T.AlignLeft]

tabLine :: Tab -> String -> Widget RName
tabLine t n = tabSelector t <+> (padLeft Max . padRight (Pad 1) $ withAttr attrAppName (str n))

tabSelector :: Tab -> Widget RName
tabSelector selected = padLeft (Pad 1) . hBox $ map render [DotfileTab, BundleTab, XMonadTab]
 where render t = toWidget True (selected == t) t
