module Tui.ThemeDemo (themeDemo) where

import           Brick.AttrMap        (AttrName, attrName)
import           Brick.Main
import           Brick.Themes         (Theme, newTheme, themeToAttrMap)
import           Brick.Types          (BrickEvent (VtyEvent), EventM, Widget)
import           Brick.Util           (fg, on)
import           Brick.Widgets.Center (center, hCenter)
import           Brick.Widgets.Core   (hLimit, str, vBox, withDefAttr, (<+>))
import           Control.Monad        (void)
import           Control.Monad.State  (put)
import           Graphics.Vty         (Event (EvKey), Key (KChar, KEsc), black,
                                       blue, green, white, yellow)

ui :: Widget n
ui = center $
     hLimit 40 $
     vBox $ hCenter <$>
          [ str "Press " <+> withDefAttr keybindingAttr (str "1") <+> str " to switch to theme 1"
          , str "Press " <+> withDefAttr keybindingAttr (str "2") <+> str " to switch to theme 2"
          ]

keybindingAttr :: AttrName
keybindingAttr = attrName "keybinding"

theme1 :: Theme
theme1 = newTheme (white `on` blue) [(keybindingAttr, fg yellow)]

theme2 :: Theme
theme2 = newTheme (green `on` black) [(keybindingAttr, fg yellow)]

appEvent :: BrickEvent () e -> EventM () Int ()
appEvent (VtyEvent (EvKey (KChar '1') [])) = put 1
appEvent (VtyEvent (EvKey (KChar '2') [])) = put 2
appEvent (VtyEvent (EvKey (KChar 'q') [])) = halt
appEvent (VtyEvent (EvKey KEsc []))        = halt
appEvent _                                 = return ()

app :: App Int e ()
app = App { appDraw = const [ui]
          , appHandleEvent = appEvent
          , appStartEvent = return ()
          , appAttrMap = \s -> themeToAttrMap $ if s == 1
                                                then theme1
                                                else theme2
          , appChooseCursor = neverShowCursor
          }

themeDemo :: IO ()
themeDemo = void $ defaultMain app 1
