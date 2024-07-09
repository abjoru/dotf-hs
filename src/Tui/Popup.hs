module Tui.Popup (
  Popup,
  popup,
  dialogL,
  widgetL,
  renderPopup,
  handlePopupEvent,
  popupSelection
) where

import           Brick                (EventM, Widget, padAll, str, vBox,
                                       vLimit, zoom)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty         as V

import           Data.List            (intercalate)
import           Lens.Micro           (Lens', lens)

data Popup a n = Popup {
  _dialog :: D.Dialog a n,
  _widget :: Widget n
}

dialogL :: Lens' (Popup a n) (D.Dialog a n)
dialogL = lens _dialog (\p d -> p { _dialog = d })

widgetL :: Lens' (Popup a n) (Widget n)
widgetL = lens _widget (\p w -> p { _widget = w })

popup :: Eq n => String -> [String] -> (n, [(String, n, a)]) -> Popup a n
popup title contentLines = popup'
  title
  ( B.border
  . vLimit (2 + length contentLines)
  . C.vCenter
  . C.hCenter
  . vBox
  $ [padAll 1 . str . intercalate "\n" $ contentLines]
  )

popup' :: Eq n => String -> Widget n -> (n, [(String, n, a)]) -> Popup a n
popup' title widget results =
  let st = " " ++ title ++ " "
      dialog = D.dialog (Just $ str st) (Just results) minPopupWidth
  in Popup dialog widget

renderPopup :: Ord n => Popup a n -> Widget n
renderPopup (Popup d w) = D.renderDialog d w

handlePopupEvent :: V.Event -> EventM n (Popup a n) ()
handlePopupEvent e = zoom dialogL $ D.handleDialogEvent e

popupSelection :: Eq n => Popup a n -> Maybe (n, a)
popupSelection (Popup d _) = D.dialogSelection d

minPopupWidth :: Int
minPopupWidth = 70
