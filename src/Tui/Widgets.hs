module Tui.Widgets where

import           Brick                      (ViewportType (Both, Vertical),
                                             Widget, hLimit, joinBorders, str,
                                             vBox, vLimit, viewport,
                                             withBorderStyle, (<+>))
import           Brick.Types                (ViewportType (Horizontal))
import           Brick.Widgets.Border       (border, borderWithLabel, hBorder,
                                             vBorder)
import           Brick.Widgets.Border.Style (unicode, unicodeBold)
import           Brick.Widgets.Center       (center, hCenter, vCenter)
import           Brick.Widgets.Core         (hBox)

data Name = VP1
          | VP2
          | VP3
          deriving (Ord, Show, Eq)

title :: String -> Widget ()
title t = withBorderStyle unicodeBold . border . vLimit 1 . vCenter . hCenter . vBox $ [str t]

sampleUi :: Widget ()
sampleUi = joinBorders
         $ withBorderStyle unicode
         $ borderWithLabel (str "Hello!") (center (str "Left") <+> vBorder <+> center (str "Right"))

sampleUi2 :: Widget Name
sampleUi2 = center
          $ border
          $ hLimit 60
          $ vLimit 21
          $ vBox [ pair, hBorder, singleton ]

pair :: Widget Name
pair = hBox [ viewport VP1 Vertical $
              vBox $ str "Press up and down array keys" :
                     str "to scroll this viewport." :
                     (str <$> [ "Line " <> show i | i <- [3..50::Int] ])
            , vBorder
            , viewport VP2 Horizontal $
              str "Press left and right arrow keys to scroll this viewport."
            ]

singleton :: Widget Name
singleton = viewport VP3 Both $
            vBox $ str "Press ctrl+arrow keys to scroll this viewport horizontally and vertically."
                 : (str <$> [ "Line " <> show i | i <- [2..25::Int] ])
