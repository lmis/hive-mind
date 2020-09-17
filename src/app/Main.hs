{-# LANGUAGE TupleSections, RecordWildCards, NamedFieldPuns, TemplateHaskell #-}
-- TODO: Track and pass random gen
-- TODO: Implement hive mind logic
-- TODO: allow environment interaction in decision
-- TODO: Cleanup
module Main
  ( main
  , Decision(..)
  , hiveMind
  , randomGenerator
  , closeHivelings
  , closeObjects
  , carriesNutrition
  , isSpreadingPheromones
  )
where

import           Data.Maybe                     ( catMaybes )
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                , elems
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
import           System.Random                  ( StdGen
                                                , mkStdGen
                                                )
import           Lens.Micro                     ( (&)
                                                , (^.)
                                                , (%~)
                                                )
import           Lens.Micro.Platform            ( makeLenses )

data AppEvent = AdvanceGame deriving (Eq, Show, Ord)
type Position = (Int, Int)
data Hiveling = Hiveling {
  _hasNutrition :: !Bool,
  _spreadsPheromones :: !Bool                                 ,
  _position :: !Position
} deriving (Eq, Show)
makeLenses ''Hiveling

data GameObject = Nutrition
                | HiveEntrance
                | Pheromone
                | Obstacle deriving (Eq, Show)

data GameState = GameState {
  _objects :: Map Position GameObject,
  _hivelings :: Map Position Hiveling,
  _score :: !Int,
  _randomGenerator :: StdGen
} deriving (Show)
makeLenses ''GameState

data HiveMindInput = HiveMindInput {
  _closeObjects :: Map Position GameObject,
  _closeHivelings :: Map Position Hiveling,
  _carriesNutrition :: !Bool,
  _isSpreadingPheromones :: !Bool
} deriving (Eq, Show)
makeLenses ''HiveMindInput


data Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Eq, Show, Ord)
data Decision = Move Direction | Pickup | Drop

type Name = ()
data AppState = AppState {
  _gameState :: !GameState,
  _iteration :: !Int
} deriving (Show)

makeLenses ''AppState

defaultHiveling :: Position -> Hiveling
defaultHiveling _position = Hiveling { _hasNutrition = False, _spreadsPheromones = False, .. }

startingState :: GameState
startingState = GameState { _objects         = Map.fromList $ entrance : boundary ++ nutrition
                          , _hivelings       = Map.fromList [ ((0, y), defaultHiveling (0, y)) | y <- [1 .. 4] ]
                          , _score           = 0
                          , _randomGenerator = mkStdGen 42
                          }
 where
  entrance = ((0, 0), HiveEntrance)
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

doGameStep :: GameState -> GameState
doGameStep s = s & hivelings %~ (takeDecisions . inputs)
 where
  inputs :: Map Position Hiveling -> [(Hiveling, HiveMindInput)]
  inputs hs = toInput <$> elems hs
  toInput :: Hiveling -> (Hiveling, HiveMindInput)
  toInput h =
    let (x, y) = h ^. position
    in
      ( h
      , HiveMindInput
        { _closeHivelings        = Map.fromList $ catMaybes
          [ ((x', y'), ) <$> ((s ^. hivelings) !? (x', y')) | x' <- [x - 2 .. x + 2], y' <- [y - 2 .. y + 2] ]
        , _closeObjects          = Map.fromList
          $ catMaybes [ ((x', y'), ) <$> ((s ^. objects) !? (x', y')) | x' <- [x - 2 .. x + 2], y' <- [y - 2 .. y + 2] ]
        , _isSpreadingPheromones = h ^. spreadsPheromones
        , _carriesNutrition      = h ^. hasNutrition
        }
      )
  takeDecisions :: [(Hiveling, HiveMindInput)] -> Map Position Hiveling
  takeDecisions inp = Map.fromList $ (\(h, i) -> (h ^. position, applyDecision (h, hiveMind i))) <$> inp
  applyDecision :: (Hiveling, Decision) -> Hiveling
  applyDecision (h, Move d) = h & position %~ move d
  applyDecision (_, _     ) = undefined

hiveMind :: HiveMindInput -> Decision
hiveMind _ = Move North

move :: Direction -> Position -> Position
move d     (x, y) = (x + dx, y + dy)
  where (dx, dy) = direction2offset d

direction2offset :: Direction -> Position
direction2offset d = case d of
            North     -> (0,-1)
            NorthEast -> (1,-1)
            East      -> (1, 0)
            SouthEast -> (1, 1)
            South     -> (0, 1)
            SouthWest -> (-1,1)
            West      -> (-1, 0)
            NorthWest -> (-1, -1)

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = halt s
handleEvent s (AppEvent AdvanceGame) = continue $ s & iteration %~ (+ 1) & gameState %~ doGameStep
handleEvent s _ = continue s

drawGameState :: GameState -> Widget Name
drawGameState g = str $ unlines [ [ renderPosition (x, y) | x <- [-10 .. 10] ] | y <- [-10 .. 10] ]
 where
  renderPosition :: Position -> Char
  renderPosition p = maybe (maybe ' ' renderObject ((g ^. objects) !? p)) renderHiveling ((g ^. hivelings) !? p)
  renderHiveling :: Hiveling -> Char
  renderHiveling h | h ^. hasNutrition && h ^. spreadsPheromones = 'û'
                   | h ^. hasNutrition      = 'î'
                   | h ^. spreadsPheromones = 'u'
                   | otherwise              = 'i'
  renderObject :: GameObject -> Char
  renderObject Nutrition    = 'N'
  renderObject HiveEntrance = 'H'
  renderObject Pheromone    = 'o'
  renderObject Obstacle     = 'X'

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ C.hCenter
      $   labeledVBox "Hive Mind"
      $   C.hCenter
      <$> [labeledVBox "Score" [padLeftRight 10 $ str . show $ s ^. gameState . score], drawGameState (s ^. gameState)]
  ]

theMap :: AttrMap
theMap = attrMap V.defAttr []

-- button :: String -> a -> Widget a
-- button label name = clickable name $ withBorderStyle BS.unicode $ B.border $ padLeftRight 1 $ str label

labeledVBox :: String -> [Widget a] -> Widget a
labeledVBox label content =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (padLeftRight 1 $ str label) $ vBox content
