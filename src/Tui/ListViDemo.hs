module Tui.ListViDemo (listViDemo) where

import           Control.Monad        (void)
import           Control.Monad.State  (modify)

import           Data.Maybe           (fromMaybe)

import qualified Graphics.Vty         as V

import           Lens.Micro           ((^.))
import           Lens.Micro.Mtl

import qualified Brick.AttrMap        as A
import qualified Brick.Main           as M
import           Brick.Types          (Widget)
import qualified Brick.Types          as T
import           Brick.Util           (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core   (hLimit, str, vBox, vLimit, withAttr,
                                       (<+>))
import qualified Brick.Widgets.List   as L

import qualified Data.Vector          as Vec

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
  where label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.L.listSelectedL of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.L.listElementsL
        box = B.borderWithLabel label $
              hLimit 25 $
              vLimit 15 $
              L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press +/- to add/remove list elements."
                              , C.hCenter $ str "Press Esc to exit."
                              ]

appEvent :: T.BrickEvent () e -> T.EventM () (L.List () Char) ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar '+') [] -> do
      els <- use L.listElementsL
      let el = nextElement els
          pos = Vec.length els
      modify $ L.listInsert pos el

    V.EvKey (V.KChar '-') [] -> do
      sel <- use L.listSelectedL
      case sel of
        Nothing -> return ()
        Just i  -> modify $ L.listRemove i

    V.EvKey V.KEsc [] -> M.halt

    ev -> L.handleListEventVi L.handleListEvent ev
  where nextElement :: Vec.Vector Char -> Char
        nextElement v = fromMaybe '?' $ Vec.find (`Vec.notElem` v) (Vec.fromList ['a'..'z'])
appEvent _ = return ()

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s = if sel
                 then withAttr customAttr (str $ "<" <> s <> ">")
                 else str s
  in C.hCenter $ str "Item " <+> selStr (show a)

initialState :: L.List () Char
initialState = L.list () (Vec.fromList ['a', 'b', 'c']) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (L.listAttr, V.white `on` V.blue)
  , (L.listSelectedAttr, V.blue `on` V.white)
  , (customAttr, fg V.cyan)
  ]

theApp :: M.App (L.List () Char) e ()
theApp = M.App { M.appDraw = drawUI
               , M.appChooseCursor = M.showFirstCursor
               , M.appHandleEvent = appEvent
               , M.appStartEvent = return ()
               , M.appAttrMap = const theMap
               }

listViDemo :: IO ()
listViDemo = void $ M.defaultMain theApp initialState
