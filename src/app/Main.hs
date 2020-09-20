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
import           Control.Monad.Trans.State      ( State
                                                , get
                                                , put
                                                , execState
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
import           Lens.Micro                     ( Lens'
                                                , Traversal'
                                                , (&)
                                                , (^?)
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
data Direction = Center | North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Eq, Show, Ord, Enum, Bounded)
data Decision = Move Direction | Pickup Direction | Drop Direction

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


doGameStep :: State GameState ()
doGameStep = do
  s <- get
  let (s', _, hivelingsWithDecision) = execState applyHiveMind (s, s ^. hivelings, [])
  put $ foldl applyDecision s' hivelingsWithDecision
  return ()
 where
  applyHiveMind :: State (GameState, [Hiveling], [(Hiveling, Decision)]) ()
  applyHiveMind = do
    (s, hs, decisions) <- get
    case hs of
      [] -> return ()
      (h : rest) ->
        let (g', g'') = split $ s ^. randomGen
            s'        = s & randomGen .~ g'
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
        in  put $ execState applyHiveMind (s', rest, (h, decision) : decisions)
       where
  applyDecision :: GameState -> (Hiveling, Decision) -> GameState
  applyDecision s (h, decision) = case decision of
    Move   d -> s & hivelings . each . withId (h ^. identifier) . position %~ tryMove s d
    Pickup d -> case targetType d of
      Just Nutrition -> if h ^. hasNutrition
        then s
        else s & currentHasNutrition .~ True & objects %~ filter (\obj -> obj ^. objectPosition /= hivelingPos `go` d)
      _ -> s
    Drop d -> case targetType d of
      Just HiveEntrance -> if h ^. hasNutrition then s & currentHasNutrition .~ False & score +~ 1 else s
      Nothing           -> s & score -~ 100 -- No food waste!
      _                 -> s
   where
    hivelingPos = h ^. position
    targetType d = objectTypeAt (hivelingPos `go` d) s
    currentHasNutrition :: Traversal' GameState Bool
    currentHasNutrition = hivelings . each . withId (h ^. identifier) . hasNutrition
  tryMove :: GameState -> Direction -> Position -> Position
  tryMove s d p =
    let next = p `go` d
    in  case objectTypeAt next s of
          Nothing       -> if has (hivelings . each . position . filtered (== next)) s then p else next
          Just Obstacle -> p
          _             -> next
  partIs :: Eq a => Lens' GameObject a -> a -> GameObject -> Bool
  partIs l x = (^. l . to (== x))
  objectTypeAt :: Position -> GameState -> Maybe ObjectType
  objectTypeAt p = (^? objects . each . filtered (objectPosition `partIs` p) . objectType)


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
      $ let minDirection = (minBound :: Direction)
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
  void $ customMain initialVty buildVty (Just chan) app AppState { _gameState = startingState, _iteration = 0, .. }
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
handleEvent s (AppEvent AdvanceGame) = continue $ s & iteration +~ 1 & gameState %~ execState doGameStep
handleEvent s _ = continue s

-- Rendering
drawGameState :: GameState -> Widget Name
drawGameState g = str $ unlines [ [ renderPosition (x, y) | x <- [-20 .. 20] ] | y <- [-20 .. 20] ]
 where
  pointsOfInterest :: Map Position Char
  pointsOfInterest =
    fromListWith const
      $  (g ^.. objects . each . to (\obj -> (obj ^. objectPosition, obj ^. objectType . to renderType)))
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
      <$> [labeledVBox "Score" [padLeftRight 10 . str . show $ s ^. gameState . score], drawGameState $ s ^. gameState]
  ]

theMap :: AttrMap
theMap = attrMap V.defAttr []

-- button :: String -> a -> Widget a
-- button label name = clickable name $ withBorderStyle BS.unicode $ B.border $ padLeftRight 1 $ str label

labeledVBox :: String -> [Widget a] -> Widget a
labeledVBox label content =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (padLeftRight 1 $ str label) $ vBox content
