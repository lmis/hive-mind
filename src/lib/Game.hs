{-# LANGUAGE TupleSections, RecordWildCards, TemplateHaskell #-}
module Game where

import           InteractiveCommand             ( InteractiveCommand
                                                , hIn
                                                , hOut
                                                )
import           Common                         ( Entity(..)
                                                , base
                                                , details
                                                , EntityDetails(..)
                                                , Position
                                                , distance
                                                , Rotation(..)
                                                , addRotations
                                                , relativePosition
                                                , Decision(..)
                                                , hPrintFlush
                                                , generatorList
                                                )
import qualified Client                         ( Input(..)
                                                , EntityBase(..)
                                                , EntityDetails'
                                                , HivelingDetails(..)
                                                )
import           GHC.Float                      ( int2Double )
import           System.IO                      ( hGetLine
                                                , hWaitForInput
                                                )
import           System.Random.Shuffle          ( shuffle' )
import           Text.Pretty.Simple             ( pShowNoColor )
import           Data.Text.Lazy                 ( unpack )
import           Safe.Foldable                  ( maximumByMay )
import           Data.Ord                       ( comparing )
import           Data.List                      ( foldl' )
import           System.Random                  ( StdGen
                                                , mkStdGen
                                                , next
                                                )
import           Control.Lens                   ( ASetter'
                                                , Prism'
                                                , prism
                                                , (&)
                                                , (^.)
                                                , (^..)
                                                , (^?)
                                                , (%~)
                                                , (.~)
                                                , (+~)
                                                , (-~)
                                                , each
                                                , filtered
                                                , _Just
                                                , makeLenses
                                                )
data EntityBase = EntityBase {
  _identifier :: !Int
 ,_position :: !Position
 ,_zIndex :: !Int
 ,_highlighted :: !Bool
} deriving (Eq, Show, Read)
makeLenses ''EntityBase

data HivelingDetails = HivelingDetails {
  _recentDecisions :: ![Decision]
 ,_memory :: !String
 ,_hasNutrition :: !Bool
 ,_spreadsPheromones :: !Bool
 ,_orientation :: !Rotation -- Rotation w.r.t North
} deriving (Eq, Show, Read)
makeLenses ''HivelingDetails

type EntityDetails' = EntityDetails HivelingDetails
type Entity' = Entity EntityBase EntityDetails'
type Hiveling' = Entity EntityBase HivelingDetails

data GameState = GameState {
  _entities :: [Entity']
 ,_nextId :: !Int
 ,_score :: !Int
 ,_randomGen :: StdGen
} deriving (Show)
makeLenses ''GameState

asHiveling :: Prism' Entity' Hiveling'
asHiveling = prism collapse refine
 where
  collapse :: Hiveling' -> Entity'
  collapse h = Entity { _base = h ^. base, _details = Hiveling $ h ^. details }
  refine :: Entity' -> Either Entity' Hiveling'
  refine (Entity b (Hiveling d)) = Right $ Entity b d
  refine e                       = Left e

sees :: Entity EntityBase d -> Position -> Bool
sees h p = distance p (h ^. base . position) < 6

addEntity :: (EntityDetails', Position) -> GameState -> GameState
addEntity (details', position') state =
  state & nextId +~ 1 & entities %~ (new :)
 where
  zIndex' =
    length
      $   state
      ^.. entities
      .   each
      .   filtered (\e -> (e ^. base . position) == position')
  new = Entity
    { _base    = EntityBase { _identifier  = state ^. nextId
                            , _highlighted = False
                            , _zIndex      = zIndex'
                            , _position    = position'
                            }
    , _details = details'
    }

startingState :: GameState
startingState =
  foldr
      addEntity
      GameState { _entities  = []
                , _nextId    = 0
                , _score     = 0
                , _randomGen = mkStdGen 42
                }
    $  ((Hiveling defaultHivelingDetails, ) <$> hivelingPositions)
    ++ ((HiveEntrance, ) <$> entrances)
    ++ ((Obstacle, ) <$> topAndBottom)
    ++ ((Obstacle, ) <$> sides)
    ++ ((Nutrition, ) <$> nutrition)
 where
  defaultHivelingDetails = HivelingDetails { _recentDecisions   = []
                                           , _memory            = ""
                                           , _hasNutrition      = False
                                           , _spreadsPheromones = False
                                           , _orientation       = None
                                           }
  hivelingPositions      = [(1, 4), (-3, 12), (0, -6), (2, 2)]
  entrances              = (,) <$> [-5, 5] <*> [-5, 5]
  topAndBottom           = (,) <$> [-9 .. 9] <*> [-16, 16]
  sides                  = (,) <$> [-10, 10] <*> [-16 .. 16]
  nutrition              = (,) <$> [-5 .. 5] <*> [-15, -14, 0, 14, 15]

doGameStep :: InteractiveCommand -> GameState -> IO (GameState, [String])
doGameStep proc state = do
  let (g : g' : generators) = generatorList $ state ^. randomGen
  let shuffledHivelings     = shuffle' hivelings (length hivelings) g

  decisionsWithMetadata <- mapM takeDecision $ zip generators shuffledHivelings

  return
    $ foldl' applyDecision (state & randomGen .~ g', []) decisionsWithMetadata
 where
  hivelings :: [Hiveling']
  hivelings = state ^.. entities . each . asHiveling
  takeDecision :: (StdGen, Hiveling') -> IO (Decision, Hiveling', Client.Input)
  takeDecision (g, hiveling) = do
    let
      (r, _) = next g
      input  = Client.Input
        { _closeEntities   =
          state
          ^.. entities
          .   each
          .   filtered
                (\e ->
                  (e ^. base . identifier) /= (hiveling ^. base . identifier)
                )
          .   filtered (\e -> hiveling `sees` (e ^. base . position))
          &   each
          %~  forHivelingMind hiveling
        , _currentHiveling = hivelingWithClientTypes hiveling
        , _randomSeed      = r
        }
    decision <- getHiveMindDecision proc input
    return (decision, hiveling, input)
  hivelingWithClientTypes :: Hiveling'
                          -> Entity Client.EntityBase Client.HivelingDetails
  hivelingWithClientTypes h =
    h
      &  base
      %~ (\EntityBase {..} -> Client.EntityBase { .. })
      &  details
      %~ \HivelingDetails {..} -> Client.HivelingDetails { .. }
  forHivelingMind :: Hiveling'
                  -> Entity EntityBase EntityDetails'
                  -> Entity Client.EntityBase Client.EntityDetails'
  forHivelingMind h e =
    e
      &  base
      %~ (\EntityBase {..} -> Client.EntityBase
           { _position =
             inverseRotatePosition (h ^. details . orientation)
               $ relativePosition (h ^. base . position) (e ^. base . position)
           , ..
           }
         )
      &  details
      .~ case e ^. details of
           Hiveling HivelingDetails {..} ->
             Hiveling Client.HivelingDetails { .. }
           Nutrition    -> Nutrition
           Obstacle     -> Obstacle
           HiveEntrance -> HiveEntrance
           Pheromone    -> Pheromone
  inverseRotatePosition :: Rotation -> Position -> Position
  inverseRotatePosition None             p      = p
  inverseRotatePosition Clockwise        (x, y) = (-y, x)
  inverseRotatePosition Counterclockwise (x, y) = (y, -x)
  inverseRotatePosition Back             (x, y) = (-x, -y)

-- TODO state monad
applyDecision :: (GameState, [String])
              -> (Decision, Hiveling', Client.Input)
              -> (GameState, [String])
applyDecision (state, logs) (decision, h, input) =
  ( (case decision of
      Remember128Characters msg ->
        state
          &  hiveling
          .  details
          .  memory
          .~ msg
          &  score
          -~ round (int2Double (length msg) / 20.0)
      Turn None -> state & score -~ 1
      Turn rotation ->
        state & hiveling . details . orientation %~ addRotations rotation
      Move   -> case topEntityAtTarget of
        Just (Entity _ Obstacle    ) -> state & score -~ 2
        Just (Entity _ (Hiveling _)) -> state
        Just (Entity b' _) ->
          state
            &  hiveling
            .  base
            %~ (position .~ targetPos)
            .  (zIndex .~ (b' ^. zIndex) + 1)
        Nothing ->
          state & hiveling . base %~ (position .~ targetPos) . (zIndex .~ 0)
      Pickup -> case topEntityAtTarget of
        Just (Entity _ Nutrition) -> if h ^. details . hasNutrition
          then state
          else
            state
            &  hiveling
            .  details
            .  hasNutrition
            .~ True
            &  entities
            %~ filter
                 (\e ->
                   Just (e ^. base . identifier)
                     /= topEntityAtTarget
                     ^? _Just
                     .  base
                     .  identifier
                 )
        _                         -> state
      Drop   -> case topEntityAtTarget of
        Just (Entity _ HiveEntrance) -> if h ^. details . hasNutrition
          then state & hiveling . details . hasNutrition .~ False & score +~ 15
          else state
        Just _                       -> state
        Nothing                      -> if h ^. details . hasNutrition
          then
            state
            &  hiveling
            .  details
            .  hasNutrition
            .~ False
            &  addEntity (Nutrition, targetPos)
          else state
    )
    &  (if h ^. details . spreadsPheromones
         then addEntity (Pheromone, h ^. base . position)
         else id
       )
    &  hiveling
    .  details
    .  recentDecisions
    %~ (\ds -> decision : take 2 ds)
  , [ "< " ++ (unpack . pShowNoColor $ decision)
    , "> " ++ (unpack . pShowNoColor $ input)
    ]
    ++ logs
  )
 where
  targetPos :: Position
  targetPos =
    let (x, y) = h ^. base . position
    in  case h ^. details . orientation of
          None             -> (x, y + 1)
          Clockwise        -> (x + 1, y)
          Back             -> (x, y - 1)
          Counterclockwise -> (x - 1, y)
  topEntityAtTarget :: Maybe Entity'
  topEntityAtTarget =
    maximumByMay (comparing (^. base . zIndex))
      $   state
      ^.. entities
      .   each
      .   filtered (\e -> (e ^. base . position) == targetPos)
      .   filtered (\e -> (e ^. base . identifier) /= (h ^. base . identifier))
  -- This could be a Traversal' but since you can get the hiveling through `h`,
  -- this should stay an ASetter'
  hiveling :: ASetter' GameState Hiveling'
  hiveling =
    entities
      . each
      . filtered (\e -> (e ^. base . identifier) == (h ^. base . identifier))
      . asHiveling


getHiveMindDecision :: InteractiveCommand -> Client.Input -> IO Decision
getHiveMindDecision cmd input = do
  hPrintFlush (cmd ^. hIn) input
  ready <- hWaitForInput (cmd ^. hOut) 100
  if ready
    then do
      output <- hGetLine (cmd ^. hOut)
      let parsed = read output
      return $ case parsed of
        Remember128Characters msg -> if length msg > 128
          then error "Attempting to remember too large messge"
          else parsed
        _                         -> parsed
    else error "mind-command decision time-out"
