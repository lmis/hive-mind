{-# LANGUAGE
   RankNTypes, TupleSections, RecordWildCards,
   NamedFieldPuns, TemplateHaskell
#-}
module Main
  ( main
  -- Unused lenses
  )
where

import           Data.IORef                     ( IORef
                                                , readIORef
                                                , modifyIORef
                                                , newIORef
                                                )
import           Safe.Foldable                  ( maximumByMay )
import           Data.Ord                       ( comparing )
import           GHC.Float                      ( int2Double )
import           Data.List                      ( find
                                                , maximumBy
                                                , sortOn
                                                )
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                , fromListWith
                                                )
import           Data.Maybe                     ( fromJust )
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
                                                , clickable
                                                , attrMap
                                                , vBox
                                                , hBox
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
                                                , _1
                                                , _2
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
 ,_zIndex :: !Int
 ,_highlighted :: !Bool
} deriving (Eq, Show)
makeLenses ''EntityBase

data EntityDetails = Hiveling HivelingProps
                   | Nutrition
                   | HiveEntrance
                   | Pheromone
                   | Obstacle deriving (Eq, Show)

type Hiveling' = (EntityBase, HivelingProps)

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

type Name = Position
data AppEvent = AdvanceGame deriving (Eq, Show, Ord)
data AppState = AppState {
  _gameState :: !GameState
 ,_running :: IORef (Bool, Int)
 ,_iteration :: !Int
}
makeLenses ''AppState

-- Traversals & Utils
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

-- Game init & advancing
addEntity :: Int -> EntityDetails -> Position -> GameState -> GameState
addEntity _zIndex _details _position state =
  state & nextId +~ 1 & entities %~ (new :)
 where
  new = Entity
    { _base = EntityBase { _identifier  = state ^. nextId
                         , _highlighted = False
                         , ..
                         }
    , ..
    }

startingState :: GameState
startingState =
  foldr
      (uncurry $ addEntity 0)
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
  topAndBottom = (,) <$> [-19 .. 19] <*> [-20, 20]
  sides        = (,) <$> [-20, 20] <*> [-20 .. 20]
  nutrition    = (,) <$> [-10 .. 10] <*> [-10, 10]

doGameStep :: GameState -> GameState
doGameStep state =
  let (hivelingWithDecision, gen) =
          runState
        -- TODO: Shuffle
                   (mapM takeDecision hivelings) (state ^. randomGen)
  in  foldl applyDecision (state & randomGen .~ gen) hivelingWithDecision
 where
  hivelings :: [Hiveling']
  hivelings = state ^.. entities . each . asHiveling . _Just
  takeDecision :: Hiveling' -> State StdGen (Hiveling', Decision)
  takeDecision hiveling = do
    g <- get
    let (g', g'') = split g
        center    = hiveling ^. _1 . position
        isClose p = distance p center < 8
        decision = hivelingMind $ HivelingMindInput
          { _closeEntities   = state
                               ^.. entities
                               .   each
                               .   filtered (isClose . (^. base . position))
                               &   each
                               %~  forHivelingMind center
          , _currentHiveling = hiveling ^. _2
          , _randomInput     = g''
          }
    put g'
    return (hiveling, decision)
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

applyDecision :: GameState -> (Hiveling', Decision) -> GameState
applyDecision state (hiveling, Decision t direction) = case t of
  Move   -> case topEntityAtTarget of
    Just (Entity _ Obstacle    ) -> state & score -~ 1
    Just (Entity _ (Hiveling _)) -> state
    Just (Entity b' _) ->
      state
        &  currentEntity
        .  base
        %~ (position .~ targetPos)
        .  (zIndex .~ (b' ^. zIndex) + 1)
    Nothing -> state & currentEntity . base . position .~ targetPos
  Pickup -> case topEntityAtTarget of
    Just (Entity _ Nutrition) -> if hiveling ^. _2 . hasNutrition
      then state
      else
        state
        &  currentHivelingProps
        .  hasNutrition
        .~ True
        &  entities
        %~ filter ((/= topEntityAtTarget) . Just)
    _                         -> state
  Drop   -> case topEntityAtTarget of
    Just (Entity _ HiveEntrance) -> if hiveling ^. _2 . hasNutrition
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
  targetPos = (hiveling ^. _1 . position) `go` direction
  topEntityAtTarget :: Maybe Entity
  topEntityAtTarget =
    maximumByMay (comparing (^. base . zIndex))
      $   state
      ^.. entities
      .   each
      .   filtered ((== targetPos) . (^. base . position))
  currentEntity :: Traversal' GameState Entity
  currentEntity =
    entities
      . each
      . filtered ((== hiveling ^. _1 . identifier) . (^. base . identifier))
  currentHivelingProps :: Traversal' GameState HivelingProps
  currentHivelingProps = currentEntity . hivelingProps . _Just


-- Hive mind
hivelingMind :: HivelingMindInput -> Decision
hivelingMind input
  | input ^. currentHiveling . hasNutrition = case
      findClose (== HiveEntrance)
    of
      Just obj -> path (obj ^. base . position) `followOrDo` Drop
      Nothing  -> randomWalk
  | otherwise = case findClose (== Nutrition) of
    Just obj -> path (obj ^. base . position) `followOrDo` Pickup
    Nothing  -> randomWalk
 where
  findClose :: (EntityDetails -> Bool) -> Maybe Entity
  findClose t = input ^? closeEntities . each . filtered (t . (^. details))
  followOrDo :: [Direction] -> DecisionType -> Decision
  followOrDo []          t = Decision t Center
  followOrDo [direction] t = Decision t direction
  followOrDo (direction : _) _ =
    if has
         ( closeEntities
         . each
         . filtered ((== direction2Offset direction) . (^. base . position))
         )
         input
      then randomWalk
      else Decision Move direction
  randomWalk :: Decision
  randomWalk =
    Decision Move
      $ let minDirection = minBound :: Direction
            maxDirection = maxBound :: Direction
            (r, _) = randomR (fromEnum minDirection, fromEnum maxDirection)
                             (input ^. randomInput)
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
  _running   <- newIORef (True, 100)
  chan       <- newBChan 10
  _          <- advanceGame _running chan

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

advanceGame :: IORef (Bool, Int) -> BChan AppEvent -> IO ThreadId
advanceGame ref chan = forkIO $ forever $ do
  runs <- readIORef ref
  case runs of
    (True, delay) -> do
      threadDelay $ delay * 1000
      writeBChan chan AdvanceGame
    _             -> return ()


setSpeed :: AppState -> Int -> EventM Name (Next AppState)
setSpeed s delay = do
  _ <- liftIO $ modifyIORef (s ^. running) (_2 .~ delay)
  continue s

handleEvent :: AppState
            -> BrickEvent Name AppEvent
            -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar ' ') []       )) = do
  _ <- liftIO $ modifyIORef (s ^. running) (_1 %~ not)
  continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar '1') []       )) = setSpeed s 300
handleEvent s (VtyEvent (V.EvKey (V.KChar '2') []       )) = setSpeed s 100
handleEvent s (VtyEvent (V.EvKey (V.KChar '3') []       )) = setSpeed s 50
handleEvent s (VtyEvent (V.EvKey (V.KChar '4') []       )) = setSpeed s 10
handleEvent s (MouseDown p V.BLeft _ _) =
  continue
    $  s
    &  gameState
    .  entities
    .  each
    .  base
    %~ (\e -> e & highlighted .~ e ^. position . to (== p))
handleEvent s (AppEvent AdvanceGame) =
  continue $ s & iteration +~ 1 & gameState %~ doGameStep
handleEvent s _ = continue s

-- Rendering
drawGameState :: GameState -> [Widget Name]
drawGameState g =
  [ hBox [ renderPosition (x, y) | x <- [-20 .. 20] ] | y <- [-20 .. 20] ]
 where
  renderPosition :: Position -> Widget Name
  renderPosition p = clickable p . str $ maybe " "
                                               (^. details . to render)
                                               (pointsOfInterest !? p)
  pointsOfInterest :: Map Position Entity
  pointsOfInterest =
    fromListWith
        (\old new -> maximumBy (comparing (^. base . zIndex)) [old, new])
      $   g
      ^.. entities
      .   each
      .   to (\e -> (e ^. base . position, e))
  render :: EntityDetails -> String
  render t = case t of
    Nutrition    -> "N"
    HiveEntrance -> "H"
    Pheromone    -> "."
    Obstacle     -> "X"
    Hiveling h | h ^. hasNutrition && h ^. spreadsPheromones -> "û"
               | h ^. hasNutrition      -> "î"
               | h ^. spreadsPheromones -> "u"
               | otherwise              -> "i"

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ C.hCenter
      .   labeledVBox "Hive Mind"
      $   C.hCenter
      <$> [ labeledVBox
              "Score"
              [padLeftRight 10 . str . show $ s ^. gameState . score]
          ]
      ++  drawGameState (s ^. gameState)
      ++  (highlightBox <$> highlights)
  ]
 where
  highlights :: [Entity]
  highlights = sortOn
    (^. base . zIndex . to negate)
    (s ^.. gameState . entities . each . filtered (^. base . highlighted))
  highlightBox :: Entity -> Widget Name
  highlightBox e = labeledVBox (e ^. base . position . to show)
                               [padLeftRight 10 . str . info $ e ^. details]
  info :: EntityDetails -> String
  info (Hiveling d) =
    unlines ["Hiveling", "Food: " ++ d ^. hasNutrition . to show]
  info d = show d

theMap :: AttrMap
theMap = attrMap V.defAttr []

labeledVBox :: String -> [Widget a] -> Widget a
labeledVBox label content =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (padLeftRight 1 $ str label)
    $ vBox content
