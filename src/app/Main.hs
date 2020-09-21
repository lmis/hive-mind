{-# LANGUAGE RankNTypes, TupleSections, RecordWildCards, NamedFieldPuns, TemplateHaskell #-}
module Main
  ( main
  -- Unused lenses
  , closeHivelings
  , isSpreadingPheromones
  )
where

import           GHC.Float                      ( int2Double )
import           Data.List                      ( find )
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                , fromListWith
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , fromJust
                                                )
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
                                                , split
                                                , randomR
                                                )
import           Lens.Micro                     ( Traversal'
                                                , (&)
                                                , (^?)
                                                , (^?!)
                                                , (^.)
                                                , (^..)
                                                , (%~)
                                                , (.~)
                                                , (+~)
                                                , (-~)
                                                , has
                                                , each
                                                , to
                                                , filtered
                                                )
import           Lens.Micro.Platform            ( makeLenses )

type Position = (Int, Int)
data Direction = Center
               | North
               | NorthEast
               | East
               | SouthEast
               | South
               | SouthWest
               | West
               | NorthWest deriving (Eq, Show, Ord, Enum, Bounded)

data Decision = Move Direction
              | Pickup Direction
              | Drop Direction deriving (Eq, Show)

data ObjectType = Nutrition
                | HiveEntrance
                | Pheromone
                | Obstacle deriving (Eq, Show)

data GameObject = GameObject {
  _objectType :: !ObjectType,
  _objectPosition :: !Position
} deriving (Eq, Show)
makeLenses ''GameObject

data Hiveling = Hiveling {
  _identifier :: !Int,
  _hasNutrition :: !Bool,
  _spreadsPheromones :: !Bool                                 ,
  _position :: !Position
} deriving (Eq, Show)
makeLenses ''Hiveling

data HiveMindInput = HiveMindInput {
  _closeObjects :: [GameObject],
  _closeHivelings :: [Hiveling],
  _carriesNutrition :: !Bool,
  _isSpreadingPheromones :: !Bool,
  _randomInput :: StdGen
} deriving (Show)
makeLenses ''HiveMindInput

data GameState = GameState {
  _objects :: [GameObject],
  _hivelings :: [Hiveling],
  _score :: !Int,
  _randomGen :: StdGen
} deriving (Show)
makeLenses ''GameState

type Name = ()
data AppEvent = AdvanceGame deriving (Eq, Show, Ord)
data AppState = AppState {
  _gameState :: !GameState,
  _iteration :: !Int
} deriving (Show)
makeLenses ''AppState

-- Traversals
withId :: Int -> Traversal' Hiveling Hiveling
withId x = filtered $ (== x) . (^. identifier)

withType :: ObjectType -> Traversal' GameObject GameObject
withType t = filtered $ (== t) . (^. objectType)

hivelingAt :: Position -> Traversal' GameState Hiveling
hivelingAt p = hivelings . each . filtered ((== p) . (^. position))

objectAt :: Position -> Traversal' GameState GameObject
objectAt p = objects . each . filtered ((== p) . (^. objectPosition))

-- Game init & advancing
spawnHiveling :: Int -> Position -> GameState -> GameState
spawnHiveling _identifier _position s = s & hivelings %~ (new :)
  where new = Hiveling { _hasNutrition = False, _spreadsPheromones = False, .. }

startingState :: GameState
startingState =
  foldr
      (uncurry spawnHiveling)
      GameState
        { _objects   = (GameObject HiveEntrance <$> entrances)
                       ++ (GameObject Obstacle <$> sides)
                       ++ (GameObject Obstacle <$> topAndBottom)
                       ++ (GameObject Nutrition <$> nutrition)
        , _hivelings = []
        , _score     = 0
        , _randomGen = mkStdGen 42
        }
    $ zip [0 ..] hivelingPositions
 where
  hivelingPositions = (1, ) <$> [1 .. 4]
  entrances         = (,) <$> [-15, 0, 15] <*> [-15, 0, 15]
  topAndBottom      = (,) <$> [-20 .. 20] <*> [-20, 20]
  sides             = (,) <$> [-20, 20] <*> [-20 .. 20]
  nutrition         = (,) <$> [-10 .. 10] <*> [-10, 10]


type Iteration a s r = ([a], s, [r]) -> ([a], s, [r])
doGameStep :: GameState -> GameState
doGameStep s =
  let (_, gen, hivelingsWithDecision) = applyHiveMind (s ^. hivelings, s ^. randomGen, [])
  in  foldl applyDecision (s & randomGen .~ gen) hivelingsWithDecision
 where
  applyHiveMind :: Iteration Hiveling StdGen (Int, Decision)
  applyHiveMind x@([], _, _) = x
  applyHiveMind (h : hs, g, decisions) =
    let (g', g'') = split g
        center    = h ^. position
        isClose p = distance p center < 8
        decision = hiveMind $ HiveMindInput
          { _closeHivelings        = s
                                     ^.. hivelings
                                     .   each
                                     .   filtered (isClose . (^. position))
                                     &   each
                                     .   position
                                     %~  relativePosition center
          , _closeObjects          = s
                                     ^.. objects
                                     .   each
                                     .   filtered (isClose . (^. objectPosition))
                                     &   each
                                     .   objectPosition
                                     %~  relativePosition center
          , _isSpreadingPheromones = h ^. spreadsPheromones
          , _carriesNutrition      = h ^. hasNutrition
          , _randomInput           = g''
          }
    in  applyHiveMind (hs, g', (h ^. identifier, decision) : decisions)

applyDecision :: GameState -> (Int, Decision) -> GameState
applyDecision s (i, decision) = case decision of
  Move d -> case s ^? objectAt (hivelingPos `go` d) . objectType of
    Nothing       -> if has (hivelingAt $ hivelingPos `go` d) s then s else moveCurrentHiveling d
    Just Obstacle -> s
    _             -> moveCurrentHiveling d
  Pickup d -> case targetType d of
    Just Nutrition -> if s ^?! currentHiveling . hasNutrition
      then s
      else s & currentHiveling . hasNutrition .~ True & objects %~ filter
        ((/= hivelingPos `go` d) . (^. objectPosition))
    _ -> s
  Drop d -> case targetType d of
    Just HiveEntrance -> if s ^?! currentHiveling . hasNutrition
      then s & currentHiveling . hasNutrition .~ False & score +~ 1
      else s
    Nothing ->
      s
        &  score
        -~ 100 -- No food waste!
        &  currentHiveling
        .  hasNutrition
        .~ False
    _ -> s
 where
  moveCurrentHiveling :: Direction -> GameState
  moveCurrentHiveling d = s & currentHiveling . position %~ (`go` d)
  hivelingPos :: Position
  hivelingPos = s ^?! currentHiveling . position
  targetType :: Direction -> Maybe ObjectType
  targetType d = s ^? objectAt (hivelingPos `go` d) . objectType
  currentHiveling :: Traversal' GameState Hiveling
  currentHiveling = hivelings . each . withId i


-- Hive mind
hiveMind :: HiveMindInput -> Decision
hiveMind h
  | h ^. carriesNutrition = case h ^? closeObjects . each . withType HiveEntrance of
    Just obj -> case obj ^. objectPosition . to offset2Direction of
      Just direction -> Drop direction
      Nothing        -> Move $ obj ^. objectPosition . to closestDirection
    Nothing -> randomWalk
  | otherwise = case h ^? closeObjects . each . withType Nutrition of
    Just obj -> case obj ^. objectPosition . to offset2Direction of
      Just direction -> Pickup direction
      Nothing        -> Move $ obj ^. objectPosition . to closestDirection
    Nothing -> randomWalk
 where
  randomWalk =
    Move
      $ let minDirection = minBound :: Direction
            maxDirection = maxBound :: Direction
            (r, _)       = randomR (fromEnum minDirection, fromEnum maxDirection) (h ^. randomInput)
        in  toEnum r

-- Direction
norm :: Position -> Double
norm (x, y) = sqrt . int2Double $ x ^ (2 :: Int) + y ^ (2 :: Int)

relativePosition :: Position -> Position -> Position
relativePosition (ox, oy) (x, y) = (x - ox, y - oy)

distance :: Position -> Position -> Double
distance p q = norm $ relativePosition p q

go :: Position -> Direction -> Position
go (x, y) d = let (dx, dy) = direction2Offset d in (x + dx, y + dy)

direction2Offset :: Direction -> Position
direction2Offset d = case d of
  Center    -> (0, 0)
  North     -> (0, -1)
  NorthEast -> (1, -1)
  East      -> (1, 0)
  SouthEast -> (1, 1)
  South     -> (0, 1)
  SouthWest -> (-1, 1)
  West      -> (-1, 0)
  NorthWest -> (-1, -1)

offset2Direction :: Position -> Maybe Direction
offset2Direction p = find ((== p) . direction2Offset) [minBound .. maxBound]

closestDirection :: Position -> Direction
closestDirection (x, y) = fromJust $ offset2Direction (limit x, limit y)
 where
  limit :: Int -> Int
  limit n = min 1 $ max (-1) n

-- App plumbing
main :: IO ()
main = do
  -- channel to inject events into main loop
  chan       <- newBChan 10
  _          <- advanceGame chan

  initialVty <- buildVty
  void $ customMain initialVty
                    buildVty
                    (Just chan)
                    app
                    AppState { _gameState = startingState, _iteration = 0, .. }
 where
  buildVty = do
    vty <- V.mkVty V.defaultConfig
    liftIO $ V.setMode (V.outputIface vty) V.Mouse True
    return vty

app :: App AppState AppEvent Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

advanceGame :: BChan AppEvent -> IO ThreadId
advanceGame chan = forkIO $ forever $ do
  writeBChan chan AdvanceGame
  threadDelay 100000

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = halt s
handleEvent s (AppEvent AdvanceGame) = continue $ s & iteration +~ 1 & gameState %~ doGameStep
handleEvent s _ = continue s

-- Rendering
drawGameState :: GameState -> Widget Name
drawGameState g = str $ unlines [ [ renderPosition (x, y) | x <- [-20 .. 20] ] | y <- [-20 .. 20] ]
 where
  pointsOfInterest :: Map Position Char
  pointsOfInterest =
    fromListWith const
      $  (g ^.. objects . each . to
           (\obj -> (obj ^. objectPosition, obj ^. objectType . to renderType))
         )
      ++ (g ^.. hivelings . each . to (\h -> (h ^. position, renderHiveling h)))
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
    Pheromone    -> '.'
    Obstacle     -> 'X'

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ C.hCenter
      .   labeledVBox "Hive Mind"
      $   C.hCenter
      <$> [ labeledVBox "Score" [padLeftRight 10 . str . show $ s ^. gameState . score]
          , drawGameState $ s ^. gameState
          ]
  ]

theMap :: AttrMap
theMap = attrMap V.defAttr []

-- button :: String -> a -> Widget a
-- button label name = clickable name $ withBorderStyle BS.unicode $ B.border $ padLeftRight 1 $ str label

labeledVBox :: String -> [Widget a] -> Widget a
labeledVBox label content =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (padLeftRight 1 $ str label) $ vBox content
