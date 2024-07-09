{-# LANGUAGE GADTs #-}
module Tui.Demo.SuspendAndResumeDemo (suspendAndResumeDemo) where

import           Control.Monad      (void)
import qualified Graphics.Vty       as V
import           Lens.Micro         ((^.))
import           Lens.Micro.TH      (makeLenses)

import           Brick.AttrMap      (attrMap)
import           Brick.Main         (App (..), defaultMain, halt,
                                     neverShowCursor, suspendAndResume)
import           Brick.Types        (BrickEvent (..), EventM, Widget)
import           Brick.Widgets.Core (str, vBox)

data St where
  St :: {_stExternalInput :: String} -> St

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [ui]
  where ui = vBox [ str $ "External input: \"" <> st ^. stExternalInput <> "\""
                  , str "(Press Esc to quit or Space to ask for input)"
                  ]

appEvent :: BrickEvent () e -> EventM () St ()
appEvent (VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> halt
  V.EvKey (V.KChar ' ') [] -> suspendAndResume $ do
    putStrLn "Suspended. Please enter something and press enter to resume:"
    s <- getLine
    return $ St { _stExternalInput = s }
  _ -> return ()
appEvent _ = return ()

initialState :: St
initialState = St { _stExternalInput = "" }

theApp :: App St e ()
theApp = App { appDraw = drawUI
             , appChooseCursor = neverShowCursor
             , appHandleEvent = appEvent
             , appStartEvent = return ()
             , appAttrMap = const $ attrMap V.defAttr []
             }

suspendAndResumeDemo :: IO ()
suspendAndResumeDemo = void $ defaultMain theApp initialState
