{-# LANGUAGE
   RankNTypes, TupleSections, RecordWildCards,
   NamedFieldPuns, TemplateHaskell
#-}
module Main
  ( main
  -- Unused lenses
  )
where

import           GHC.Float                      ( int2Double )
import           Data.List                      ( find )
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                , fromList
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
                                                , lens
                                                , filtered
                                                , _Just
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

data HivelingProps = HivelingProps {
 _hasNutrition :: !Bool
 ,_spreadsPheromones :: !Bool
} deriving (Eq, Show)
makeLenses ''HivelingProps

data EntityBase = EntityBase {
  _identifier :: !Int
 ,_position :: !Position
} deriving (Eq, Show)
makeLenses ''EntityBase

data EntityDetails = Hiveling HivelingProps
                   | Nutrition
                   | HiveEntrance
                   | Pheromone
                   | Obstacle deriving (Eq, Show)

data Entity = Entity {
 _base :: !EntityBase
 ,_details :: !EntityDetails
} deriving (Eq, Show)
makeLenses ''Entity

data HivelingMindInput = HivelingMindInput {
  _closeEntities :: [Entity]
 ,_currentHiveling :: HivelingProps
 ,_randomInput :: StdGen
} deriving (Show)
makeLenses ''HivelingMindInput

data GameState = GameState {
  _entities :: [Entity]
 ,_nextId :: Int
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

-- Traversals & Utils
withId :: Int -> Traversal' Entity Entity
withId x = filtered $ (== x) . (^. base . identifier)

asHiveling :: Lens' Entity (Maybe (EntityBase, HivelingProps))
asHiveling = lens getter setter
 where
  getter :: Entity -> Maybe (EntityBase, HivelingProps)
  getter (Entity b (Hiveling d)) = Just (b, d)
  getter _                       = Nothing
  setter :: Entity -> Maybe (EntityBase, HivelingProps) -> Entity
  setter (Entity _ (Hiveling _)) (Just (b, d)) = Entity b (Hiveling d)
  setter e                       _             = e

hivelingProps :: Lens' Entity (Maybe HivelingProps)
hivelingProps = lens getter setter
 where
  getter :: Entity -> Maybe HivelingProps
  getter (Entity _ (Hiveling d)) = Just d
  getter _                       = Nothing
  setter :: Entity -> Maybe HivelingProps -> Entity
  setter (Entity b (Hiveling _)) (Just d) = Entity b (Hiveling d)
  setter e                       _        = e

entityAt :: Position -> Traversal' GameState Entity
entityAt p = entities . each . filtered ((== p) . (^. base . position))

-- Game init & advancing
addEntity :: EntityDetails -> Position -> GameState -> GameState
addEntity d p state = state & nextId +~ 1 & entities %~ (new :)
  where new = Entity (EntityBase (state ^. nextId) p) d

startingState :: GameState
startingState =
  foldr
      (uncurry addEntity)
      GameState { _entities  = []
                , _nextId    = 0
                , _score     = 0
                , _randomGen = mkStdGen 42
                }
    $  ((Hiveling $ HivelingProps False False, ) <$> hivelings)
    ++ ((HiveEntrance, ) <$> entrances)
    ++ ((Obstacle, ) <$> topAndBottom)
    ++ ((Obstacle, ) <$> sides)
    ++ ((Nutrition, ) <$> nutrition)
 where
  hivelings    = (1, ) <$> [1 .. 4]
  entrances    = (,) <$> [-15, 0, 15] <*> [-15, 0, 15]
  topAndBottom = (,) <$> [-20 .. 20] <*> [-20, 20]
  sides        = (,) <$> [-20, 20] <*> [-20 .. 20]
  nutrition    = (,) <$> [-10 .. 10] <*> [-10, 10]

doGameStep :: GameState -> GameState
doGameStep s =
  let (hivelingWithDecision, gen) =
          runState
        -- TODO: Shuffle
                   (mapM takeDecision hivelings) (s ^. randomGen)
  in  foldl applyDecision (s & randomGen .~ gen) hivelingWithDecision
 where
  hivelings :: [(EntityBase, HivelingProps)]
  hivelings = s ^.. entities . each . asHiveling . _Just
  takeDecision :: (EntityBase, HivelingProps)
               -> State StdGen (EntityBase, HivelingProps, Decision)
  takeDecision (b, h) = do
    g <- get
    let (g', g'') = split g
        center    = b ^. position
        isClose p = distance p center < 8
        decision = hivelingMind $ HivelingMindInput
          { _closeEntities   = s
                               ^.. entities
                               .   each
                               .   filtered (isClose . (^. base . position))
                               &   each
                               %~  forHivelingMind center
          , _currentHiveling = h
          , _randomInput     = g''
          }
    put g'
    return (b, h, decision)
  forHivelingMind :: Position -> Entity -> Entity
  forHivelingMind center e =
    e
      &  base
      .  position
      %~ relativePosition center
      &
      -- Hivelings don't know about ids
         base
      .  identifier
      .~ -1

applyDecision :: GameState -> (EntityBase, HivelingProps, Decision) -> GameState
applyDecision state (b, h, Decision t direction) = case t of
  Move   -> case entityAtTarget of
    Just (Entity _ Obstacle) -> state & score -~ 1
    Just _ -> state
    Nothing -> state & currentEntity . base . position .~ targetPos
  Pickup -> case entityAtTarget of
    Just (Entity _ Nutrition) -> if h ^. hasNutrition
      then state
      else
        state
        &  currentHivelingProps
        .  hasNutrition
        .~ True
        &  entities
        %~ filter ((/= entityAtTarget) . Just)
    _                         -> state
  Drop   -> case entityAtTarget of
    Just (Entity _ HiveEntrance) -> if h ^. hasNutrition
      then state & currentHivelingProps . hasNutrition .~ False & score +~ 10
      else state
    Just _                       -> state
    Nothing ->
      state
        &  score
        -~ 100 -- No food waste!
        &  currentHivelingProps
        .  hasNutrition
        .~ False
 where
  targetPos :: Position
  targetPos = (b ^. position) `go` direction
  entityAtTarget :: Maybe Entity
  entityAtTarget = state ^? entityAt targetPos
  currentEntity :: Traversal' GameState Entity
  currentEntity = entities . each . withId (b ^. identifier)
  currentHivelingProps :: Traversal' GameState HivelingProps
  currentHivelingProps = currentEntity . hivelingProps . _Just


-- Hive mind
hivelingMind :: HivelingMindInput -> Decision
hivelingMind h
  | h ^. currentHiveling . hasNutrition = case findClose (== HiveEntrance) of
    Just obj -> path (obj ^. base . position) `followOrDo` Drop
    Nothing  -> randomWalk
  | otherwise = case findClose (== Nutrition) of
    Just obj -> path (obj ^. base . position) `followOrDo` Pickup
    Nothing  -> randomWalk
 where
  findClose :: (EntityDetails -> Bool) -> Maybe Entity
  findClose t = h ^? closeEntities . each . filtered (t . (^. details))
  followOrDo :: [Direction] -> DecisionType -> Decision
  followOrDo []          t = Decision t Center
  followOrDo [direction] t = Decision t direction
  followOrDo (direction : _) _
    | has
      ( closeEntities
      . each
      . filtered ((== direction2Offset direction) . (^. base . position))
      )
      h
    = randomWalk
    | otherwise
    = Decision Move direction
  randomWalk :: Decision
  randomWalk =
    Decision Move
      $ let minDirection = minBound :: Direction
            maxDirection = maxBound :: Direction
            (r, _) = randomR (fromEnum minDirection, fromEnum maxDirection)
                             (h ^. randomInput)
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
    -- Grab mouse
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
  threadDelay $ 100 * 1000

handleEvent :: AppState
            -> BrickEvent Name AppEvent
            -> EventM Name (Next AppState)
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
  renderPosition :: Position -> Char
  renderPosition p = fromMaybe ' ' (pointsOfInterest !? p)
  pointsOfInterest :: Map Position Char
  pointsOfInterest =
    fromList
      $   g
      ^.. entities
      .   each
      .   to (\e -> (e ^. base . position, render $ e ^. details))
  render :: EntityDetails -> Char
  render t = case t of
    Nutrition    -> 'N'
    HiveEntrance -> 'H'
    Pheromone    -> '.'
    Obstacle     -> 'X'
    Hiveling h | h ^. hasNutrition && h ^. spreadsPheromones -> 'û'
               | h ^. hasNutrition      -> 'î'
               | h ^. spreadsPheromones -> 'u'
               | otherwise              -> 'i'

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

labeledVBox :: String -> [Widget a] -> Widget a
labeledVBox label content =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (padLeftRight 1 $ str label)
    $ vBox content
