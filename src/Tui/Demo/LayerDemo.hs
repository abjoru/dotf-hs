module Tui.Demo.LayerDemo (layerDemo) where

import           Brick.AttrMap        (AttrName, attrMap, attrName)
import qualified Brick.Main           as M
import           Brick.Types          (Location (..), Widget, locationColumnL,
                                       locationRowL)
import qualified Brick.Types          as T
import           Brick.Util           (fg)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core   (relativeTo, reportExtent, str,
                                       translateBy, withDefAttr)
import           Control.Monad        (void)
import qualified Graphics.Vty         as V
import           Lens.Micro           ((^.))
import           Lens.Micro.Mtl
import           Lens.Micro.TH        (makeLenses)

data St = St {
  _middleLayerLocation :: T.Location,
  _bottomLayerLocation :: T.Location
}

makeLenses ''St

data Name = MiddleLayerElement
  deriving (Ord, Eq, Show)

drawUI :: St -> [Widget Name]
drawUI st = [ C.centerLayer $
              B.border $
              str "This layer is centered but other\nlayers are placed underneath it."
            , arrowLayer
            , middleLayer st
            , bottomLayer st
            ]

arrowLayer :: Widget Name
arrowLayer =
  let msg = "Relatively\n" <>
            "positioned\n" <>
            "arrow---->"
  in relativeTo MiddleLayerElement (Location (-10, -2)) $
     withDefAttr arrowAttr $
     str msg

middleLayer :: St -> Widget Name
middleLayer st = translateBy (st ^. middleLayerLocation) $
                 reportExtent MiddleLayerElement $
                 B.border $ str "Middle layer\n(Arrow keys move)"

bottomLayer :: St -> Widget Name
bottomLayer st = translateBy (st ^. bottomLayerLocation) $
                 B.border $ str "Bottom layer\n(Ctrl-arrow keys move)"

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KDown [])) = middleLayerLocation.locationRowL %= (+ 1)
appEvent (T.VtyEvent (V.EvKey V.KUp [])) = middleLayerLocation.locationRowL %= (subtract 1)
appEvent (T.VtyEvent (V.EvKey V.KRight [])) = middleLayerLocation.locationColumnL %= (+ 1)
appEvent (T.VtyEvent (V.EvKey V.KLeft [])) = middleLayerLocation.locationColumnL %= (subtract 1)
appEvent (T.VtyEvent (V.EvKey V.KDown [V.MCtrl])) = bottomLayerLocation.locationRowL %= (+ 1)
appEvent (T.VtyEvent (V.EvKey V.KUp [V.MCtrl])) = bottomLayerLocation.locationRowL %= (subtract 1)
appEvent (T.VtyEvent (V.EvKey V.KRight [V.MCtrl])) = bottomLayerLocation.locationColumnL %= (+ 1)
appEvent (T.VtyEvent (V.EvKey V.KLeft [V.MCtrl])) = bottomLayerLocation.locationColumnL %= (subtract 1)
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent _ = return ()

arrowAttr :: AttrName
arrowAttr = attrName "attr"

app :: M.App St e Name
app = M.App { M.appDraw = drawUI
            , M.appStartEvent = return ()
            , M.appHandleEvent = appEvent
            , M.appAttrMap = const $ attrMap V.defAttr [(arrowAttr, fg V.cyan)]
            , M.appChooseCursor = M.neverShowCursor
            }

layerDemo :: IO ()
layerDemo = void $ M.defaultMain app $ St (T.Location (20, 5)) (T.Location (0, 0))
