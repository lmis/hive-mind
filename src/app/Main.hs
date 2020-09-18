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

import           GHC.Float                      ( int2Double )
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                , fromListWith
                                                )
import           Data.Maybe                     ( fromMaybe )
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
                                                , (^..)
                                                , (%~)
                                                , each
                                                , to
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

data ObjectType = Nutrition
                | HiveEntrance
                | Pheromone
                | Obstacle deriving (Eq, Show)

data GameObject = GameObject {
  _objectType :: !ObjectType,
  _objectPosition :: !Position
} deriving (Eq, Show)
makeLenses ''GameObject

data GameState = GameState {
  _objects :: [GameObject],
  _hivelings :: [Hiveling],
  _score :: !Int,
  _randomGenerator :: StdGen
} deriving (Show)
makeLenses ''GameState

data HiveMindInput = HiveMindInput {
  _closeObjects :: [GameObject],
  _closeHivelings :: [Hiveling],
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
startingState = GameState { _objects         = entrance : boundary ++ nutrition
                          , _hivelings       = [ defaultHiveling (1, y) | y <- [1 .. 4] ]
                          , _score           = 0
                          , _randomGenerator = mkStdGen 42
                          }
 where
  entrance  = GameObject HiveEntrance (0, 0)
  boundary  = GameObject Obstacle <$> ((,) <$> [-10 .. 10] <*> [-10, 10]) ++ ((,) <$> [-10, 10] <*> [-10 .. 10])
  nutrition = GameObject Nutrition . (9, ) <$> [-10 .. 0]

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
  void $ customMain initialVty buildVty (Just chan) app AppState { _gameState = startingState, _iteration = 0, .. }
 where
  buildVty = do
    vty <- V.mkVty V.defaultConfig
    liftIO $ V.setMode (V.outputIface vty) V.Mouse True
    return vty

advanceGame :: BChan AppEvent -> IO ThreadId
advanceGame chan = forkIO $ forever $ do
  writeBChan chan AdvanceGame
  threadDelay 100000

norm :: Position -> Double
norm (x, y) = sqrt . int2Double $ x ^ (2 :: Int) + y ^ (2 :: Int)

relativePosition :: Position -> Position -> Position
relativePosition (ox, oy) (x, y) = (x - ox, y - oy)

distance :: Position -> Position -> Double
distance p q = norm $ relativePosition p q

doGameStep :: GameState -> GameState
doGameStep s = s & hivelings %~ (takeDecisions . map addInput)
 where
  addInput :: Hiveling -> (Hiveling, HiveMindInput)
  addInput h =
    ( h
    , HiveMindInput { _closeHivelings        = filter (\other -> isClose h $ other ^. position) (s ^. hivelings)
                    , _closeObjects          = filter (\obj -> isClose h $ obj ^. objectPosition) (s ^. objects)
                    , _isSpreadingPheromones = h ^. spreadsPheromones
                    , _carriesNutrition      = h ^. hasNutrition
                    }
    )
  isClose :: Hiveling -> Position -> Bool
  isClose h p = distance p (h ^. position) < 2
  takeDecisions :: [(Hiveling, HiveMindInput)] -> [Hiveling]
  takeDecisions = map $ \(h, i) -> applyDecision (h, hiveMind i)
  applyDecision :: (Hiveling, Decision) -> Hiveling
  applyDecision (h, Move d) = h & position %~ tryMove d
  applyDecision (_, _     ) = undefined --TODO
  tryMove :: Direction -> Position -> Position
  tryMove d p
    | next `notElem` (s ^.. hivelings . each . position) && next `notElem` (s ^.. objects . each . objectPosition) = next
    | otherwise = p
    where next = move d p

hiveMind :: HiveMindInput -> Decision
hiveMind _ = Move North

move :: Direction -> Position -> Position
move d (x, y) = (x + dx, y + dy) where (dx, dy) = direction2offset d

direction2offset :: Direction -> Position
direction2offset d = case d of
  North     -> (0, -1)
  NorthEast -> (1, -1)
  East      -> (1, 0)
  SouthEast -> (1, 1)
  South     -> (0, 1)
  SouthWest -> (-1, 1)
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
  pointsOfInterest :: Map Position Char
  pointsOfInterest =
    fromListWith const
      $  (g ^.. hivelings . each . to (\h -> (h ^. position, renderHiveling h)))
      ++ (g ^.. objects . each . to (\obj -> (obj ^. objectPosition, obj ^. objectType . to renderType)))
  renderPosition :: Position -> Char
  renderPosition p = fromMaybe ' ' (pointsOfInterest !? p)
  renderHiveling :: Hiveling -> Char
  renderHiveling h | h ^. hasNutrition && h ^. spreadsPheromones = 'û'
                   | h ^. hasNutrition      = 'î'
                   | h ^. spreadsPheromones = 'u'
                   | otherwise              = 'i'
  renderType :: ObjectType -> Char
  renderType t = case t of
    Nutrition    -> 'N'
    HiveEntrance -> 'H'
    Pheromone    -> 'o'
    Obstacle     -> 'X'

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ C.hCenter
      .   labeledVBox "Hive Mind"
      $   C.hCenter
      <$> [labeledVBox "Score" [padLeftRight 10 . str . show $ s ^. gameState . score], drawGameState $ s ^. gameState]
  ]

theMap :: AttrMap
theMap = attrMap V.defAttr []

-- button :: String -> a -> Widget a
-- button label name = clickable name $ withBorderStyle BS.unicode $ B.border $ padLeftRight 1 $ str label

labeledVBox :: String -> [Widget a] -> Widget a
labeledVBox label content =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (padLeftRight 1 $ str label) $ vBox content
