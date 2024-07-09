module Tui.Demo.DialogDemo (dialogDemo) where

import qualified Brick.AttrMap        as A
import qualified Brick.Main           as M
import           Brick.Types          (BrickEvent (..), Widget)
import qualified Brick.Types          as T
import           Brick.Util           (bg, on)
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core   (padAll, str)
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty         as V

data Choice = Red | Blue | Green
  deriving Show

data Name = RedButton
          | BlueButton
          | GreenButton
          deriving (Show, Eq, Ord)

drawUI :: D.Dialog Choice Name -> [Widget Name]
drawUI d = [ui]
  where ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "This is the dialog body."

appEvent :: BrickEvent Name e -> T.EventM Name (D.Dialog Choice Name) ()
appEvent (VtyEvent ev) = case ev of
  V.EvKey V.KEsc []   -> M.halt
  V.EvKey V.KEnter [] -> M.halt
  _                   -> D.handleDialogEvent ev
appEvent _ = return ()

initialState :: D.Dialog Choice Name
initialState = D.dialog (Just $ str "Title") (Just (RedButton, choices)) 70
  where choices = [ ("Red", RedButton, Red)
                  , ("Blue", BlueButton, Blue)
                  , ("Green", GreenButton, Green)
                  ]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (D.dialogAttr, V.white `on` V.blue)
  , (D.buttonAttr, V.black `on` V.white)
  , (D.buttonSelectedAttr, bg V.yellow)
  ]

theApp :: M.App (D.Dialog Choice Name) e Name
theApp = M.App { M.appDraw = drawUI
               , M.appChooseCursor = M.showFirstCursor
               , M.appHandleEvent = appEvent
               , M.appStartEvent = return ()
               , M.appAttrMap = const theMap
               }

dialogDemo :: IO ()
dialogDemo = do
  d <- M.defaultMain theApp initialState
  putStrLn $ "You chose: " <> show (D.dialogSelection d)
