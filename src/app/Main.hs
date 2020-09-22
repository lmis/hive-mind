{-# LANGUAGE
   RankNTypes, TupleSections, RecordWildCards,
   NamedFieldPuns, TemplateHaskell
#-}
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
import           Control.Monad.Trans.State      ( State
                                                , get
                                                , put
                                                , runState
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

data DecisionType = Move
                  | Pickup
                  | Drop deriving (Eq, Show)

data Decision = Decision DecisionType Direction deriving (Eq, Show)

data ObjectType = Nutrition
                | HiveEntrance
                | Pheromone
                | Obstacle deriving (Eq, Show)

data GameObject = GameObject {
  _objectType :: !ObjectType
 ,_objectPosition :: !Position
} deriving (Eq, Show)
makeLenses ''GameObject

data Hiveling = Hiveling {
  _identifier :: !Int
  ,_hasNutrition :: !Bool
  ,_spreadsPheromones :: !Bool
  ,_position :: !Position
} deriving (Eq, Show)
makeLenses ''Hiveling

data HivelingMindInput = HivelingMindInput {
  _closeObjects :: [GameObject]
 ,_closeHivelings :: [Hiveling]
 ,_carriesNutrition :: !Bool
 ,_isSpreadingPheromones :: !Bool
 ,_randomInput :: StdGen
} deriving (Show)
makeLenses ''HivelingMindInput

data GameState = GameState {
  _objects :: [GameObject]
 ,_hivelings :: [Hiveling]
 ,_score :: !Int
 ,_randomGen :: StdGen
} deriving (Show)
makeLenses ''GameState

type Name = ()
data AppEvent = AdvanceGame deriving (Eq, Show, Ord)
data AppState = AppState {
  _gameState :: !GameState
 ,_iteration :: !Int
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
 where
  new = Hiveling { _hasNutrition = False, _spreadsPheromones = False, .. }

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


doGameStep :: GameState -> GameState
doGameStep s =
  let (hivelingsWithDecision, gen) =
          runState (mapM takeDecision $ s ^. hivelings) (s ^. randomGen)
  in  foldl applyDecision (s & randomGen .~ gen) hivelingsWithDecision
 where
  takeDecision :: Hiveling -> State StdGen (Int, Decision)
  takeDecision h = do
    g <- get
    let (g', g'') = split g
        center    = h ^. position
        isClose p = distance p center < 8
        decision = hivelingMind $ HivelingMindInput
          { _closeHivelings        = s
                                     ^.. hivelings
                                     .   each
                                     .   filtered (isClose . (^. position))
                                     &   each
                                     .   position
                                     %~  relativePosition center
                                     &   each
                                     .   identifier
                                     .~  -1
          , _closeObjects          = s
                                     ^.. objects
                                     .   each
                                     . filtered (isClose . (^. objectPosition))
                                     &   each
                                     .   objectPosition
                                     %~  relativePosition center
          , _isSpreadingPheromones = h ^. spreadsPheromones
          , _carriesNutrition      = h ^. hasNutrition
          , _randomInput           = g''
          }
    put g'
    return (h ^. identifier, decision)

applyDecision :: GameState -> (Int, Decision) -> GameState
applyDecision state (i, Decision t direction) = case t of
  Move -> case targetObject of
    Just Obstacle -> state
    _ | has (hivelingAt targetPos) state -> state
      | otherwise -> state & currentHiveling . position .~ targetPos
  Pickup -> case targetObject of
    Just Nutrition
      | state ^?! currentHiveling . hasNutrition
      -> state
      | otherwise
      -> state & currentHiveling . hasNutrition .~ True & objects %~ filter
        ((/= targetPos) . (^. objectPosition))
    Just _  -> state
    Nothing -> state
  Drop -> case targetObject of
    Just HiveEntrance
      | state ^?! currentHiveling . hasNutrition
      -> state & currentHiveling . hasNutrition .~ False & score +~ 1
      | otherwise
      -> state
    Just _ -> state
    Nothing ->
      state
        &  score
        -~ 100 -- No food waste!
        &  currentHiveling
        .  hasNutrition
        .~ False
 where
  currentHiveling :: Traversal' GameState Hiveling
  currentHiveling = hivelings . each . withId i
  hivelingPos :: Position
  hivelingPos = state ^?! currentHiveling . position
  targetPos :: Position
  targetPos = hivelingPos `go` direction
  targetObject :: Maybe ObjectType
  targetObject = state ^? objectAt targetPos . objectType


-- Hive mind
hivelingMind :: HivelingMindInput -> Decision
hivelingMind h
  | h ^. carriesNutrition = case findClose HiveEntrance of
    Just obj -> path (obj ^. objectPosition) `followOrDo` Drop
    Nothing  -> randomWalk
  | otherwise = case findClose Nutrition of
    Just obj -> path (obj ^. objectPosition) `followOrDo` Pickup
    Nothing  -> randomWalk
 where
  findClose :: ObjectType -> Maybe GameObject
  findClose t = h ^? closeObjects . each . withType t
  followOrDo :: [Direction] -> DecisionType -> Decision
  followOrDo []          t = Decision t Center
  followOrDo [direction] t = Decision t direction
  followOrDo (direction : _) _
    | has
      (closeHivelings . each . filtered
        ((== direction2Offset direction) . (^. position))
      )
      h
    = randomWalk
    | otherwise
    = Decision Move direction
  randomWalk :: Decision
  randomWalk =
    Decision Move
      $ let
          minDirection = minBound :: Direction
          maxDirection = maxBound :: Direction
          (r, _)       = randomR (fromEnum minDirection, fromEnum maxDirection)
                                 (h ^. randomInput)
        in
          toEnum r

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

path :: Position -> [Direction]
path p@(x, y) = case offset2Direction p of
  Just d -> [d]
  _ ->
    let dir      = closestDirection p
        (x', y') = direction2Offset dir
        remain   = (x - x', y - y')
    in  dir : path remain

-- App plumbing
main :: IO ()
main = do
  -- channel to inject events into main loop
  chan       <- newBChan 10
  _          <- advanceGame chan

  initialVty <- buildVty
  void $ customMain
    initialVty
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

handleEvent
  :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = halt s
handleEvent s (AppEvent AdvanceGame) =
  continue $ s & iteration +~ 1 & gameState %~ doGameStep
handleEvent s _ = continue s

-- Rendering
drawGameState :: GameState -> Widget Name
drawGameState g = str
  $ unlines [ [ renderPosition (x, y) | x <- [-20 .. 20] ] | y <- [-20 .. 20] ]
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
      <$> [ labeledVBox
            "Score"
            [padLeftRight 10 . str . show $ s ^. gameState . score]
          , drawGameState $ s ^. gameState
          ]
  ]

theMap :: AttrMap
theMap = attrMap V.defAttr []

-- button :: String -> a -> Widget a
-- button label name = clickable name $ withBorderStyle BS.unicode $ B.border $ padLeftRight 1 $ str label

labeledVBox :: String -> [Widget a] -> Widget a
labeledVBox label content =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (padLeftRight 1 $ str label)
    $ vBox content
