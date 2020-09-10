{-# LANGUAGE RecordWildCards, NamedFieldPuns, TemplateHaskell #-}
module Main
  ( main
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Concurrent             ( ThreadId
                                                , threadDelay
                                                , forkIO
                                                )
import           Brick                          ( App(..)
                                                , AttrMap
                                                , BrickEvent(..)
                                                , EventM
                                                , Next
                                                , Widget
                                                , customMain
                                                , neverShowCursor
                                                , continue
                                                , halt
                                                , attrMap
                                                , vBox
                                                , withBorderStyle
                                                , str
                                                , padLeftRight
                                                )
import qualified Brick.Widgets.Center          as C
import qualified Graphics.Vty                  as V
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import           Brick.BChan                    ( BChan
                                                , newBChan
                                                , writeBChan
                                                )
import           Lens.Micro                     ( (&), (^.), (%~) )
import           Lens.Micro.Platform            ( makeLenses )

data AppEvent = AdvanceGame deriving (Eq, Show, Ord)
type GameState = ()
type Name = ()
data AppState = AppState {
  _gameState :: !GameState,
  _iteration :: !Int
} deriving (Eq, Show)

makeLenses ''AppState

app :: App AppState AppEvent Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

main :: IO ()
main = do
  -- channel to inject events into main loop
  chan         <- newBChan 10
  _            <- advanceGame chan

  initialVty   <- buildVty
  void $ customMain initialVty buildVty (Just chan) app $ AppState { _gameState = (), _iteration=0, .. }
 where
  buildVty = do
    vty <- V.mkVty V.defaultConfig
    liftIO $ V.setMode (V.outputIface vty) V.Mouse True
    return vty

advanceGame :: BChan AppEvent -> IO ThreadId
advanceGame chan = forkIO $ forever $ do
  writeBChan chan AdvanceGame
  threadDelay 100000


handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = halt s
handleEvent s (AppEvent AdvanceGame) = continue $ s & iteration %~ (+1)
handleEvent s _ = continue s

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ C.vCenter $ vBox
      (C.hCenter <$> [labeledVBox "Hive Mind" $ C.hCenter <$> [str $ "Hello: " ++ show (s ^. gameState) ++ show (s ^. iteration)]])
  ]

theMap :: AttrMap
theMap = attrMap V.defAttr []

-- button :: String -> a -> Widget a
-- button label name = clickable name $ withBorderStyle BS.unicode $ B.border $ padLeftRight 1 $ str label

labeledVBox :: String -> [Widget a] -> Widget a
labeledVBox label content =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (padLeftRight 1 $ str label) $ vBox content
