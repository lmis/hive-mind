{-# LANGUAGE RecordWildCards, NamedFieldPuns, TemplateHaskell #-}
module Main
  ( main
  )
where

import           Data.Map.Strict                ( Map
                                                , (!?)
                                                )
import qualified Data.Map.Strict               as Map
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
import           Lens.Micro                     ( (&)
                                                , (^.)
                                                , (%~)
                                                )
import           Lens.Micro.Platform            ( makeLenses )

data AppEvent = AdvanceGame deriving (Eq, Show, Ord)
data GameObject = Hiveling | Nutrition | HiveEntrance | Pheromone | Obstacle deriving (Eq, Show, Ord)
type Position = (Int, Int)
newtype GameState = GameState {
  _objects :: Map Position GameObject
} deriving (Eq, Show)
makeLenses ''GameState
type Name = ()
data AppState = AppState {
  _gameState :: !GameState,
  _iteration :: !Int
} deriving (Eq, Show)

makeLenses ''AppState

startingState :: GameState
startingState = GameState { _objects = Map.fromList $ entrance : boundary ++ hivelings ++ nutrition }
 where
  entrance  = ((0, 0), HiveEntrance)
  hivelings = [ ((0, y), Hiveling) | y <- [1 .. 4] ]
  boundary =
    [ ((x, y), Obstacle) | x <- [-10 .. 10], y <- [-10, 10] ]
      ++ [ ((x, y), Obstacle) | x <- [-10, 10], y <- [-10 .. 10] ]
  nutrition = [ ((9, y), Nutrition) | y <- [-10 .. 0] ]

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
  chan       <- newBChan 10
  _          <- advanceGame chan

  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app $ AppState { _gameState = startingState, _iteration = 0, .. }
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
handleEvent s (AppEvent AdvanceGame) = continue $ s & iteration %~ (+ 1)
handleEvent s _ = continue s

drawGameState :: GameState -> [Widget Name]
drawGameState g = [str $ unlines [ [ renderPosition (x, y) | x <- [-10 .. 10] ] | y <- [-10 .. 10] ]]
 where
  renderPosition :: Position -> Char
  renderPosition (x, y) = maybe ' ' symbol ((g ^. objects) !? (x, y))
  symbol :: GameObject -> Char
  symbol Hiveling     = 'H'
  symbol Nutrition    = 'N'
  symbol HiveEntrance = 'E'
  symbol Pheromone    = 'P'
  symbol Obstacle     = 'X'

drawUI :: AppState -> [Widget Name]
drawUI s = [C.vCenter $ vBox (C.hCenter <$> [labeledVBox "Hive Mind" $ C.hCenter <$> drawGameState (s ^. gameState)])]

theMap :: AttrMap
theMap = attrMap V.defAttr []

-- button :: String -> a -> Widget a
-- button label name = clickable name $ withBorderStyle BS.unicode $ B.border $ padLeftRight 1 $ str label

labeledVBox :: String -> [Widget a] -> Widget a
labeledVBox label content =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (padLeftRight 1 $ str label) $ vBox content
