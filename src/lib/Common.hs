{-# LANGUAGE TemplateHaskell #-}
module Common where

import           GHC.Float                      ( int2Double )
import           Data.List                      ( find )
import           Data.Maybe                     ( fromJust )
import           Lens.Micro                     ( Lens'
                                                , lens
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
               | NorthWest deriving (Eq, Show, Read, Ord, Enum, Bounded)

data DecisionType = Move
                  | Pickup
                  | Drop deriving (Eq, Show, Read)

data Decision = Decision DecisionType Direction deriving (Eq, Show, Read)

data HivelingProps = HivelingProps {
  _lastDecision :: !Decision
 ,_hasNutrition :: !Bool
 ,_spreadsPheromones :: !Bool
} deriving (Eq, Show, Read)
makeLenses ''HivelingProps

data EntityBase = EntityBase {
  _identifier :: !Int
 ,_position :: !Position
 ,_zIndex :: !Int
 ,_highlighted :: !Bool
} deriving (Eq, Show, Read)
makeLenses ''EntityBase

data EntityDetails = Hiveling HivelingProps
                   | Nutrition
                   | HiveEntrance
                   | Pheromone
                   | Obstacle deriving (Eq, Show, Read)

type Hiveling' = (EntityBase, HivelingProps)

data Entity = Entity {
  _base :: !EntityBase
 ,_details :: !EntityDetails
} deriving (Eq, Show, Read)
makeLenses ''Entity

data HivelingMindInput = HivelingMindInput {
  _closeEntities :: [Entity]
 ,_currentHiveling :: HivelingProps
 ,_randomSeed :: Int
} deriving (Eq, Show, Read)
makeLenses ''HivelingMindInput

-- Traversals & Utils
is :: Eq b => (a -> b) -> b -> (a -> Bool)
is f t = (== t) . f

isNot :: Eq b => (a -> b) -> b -> (a -> Bool)
isNot f t = (/= t) . f

asHiveling :: Lens' Entity (Maybe Hiveling')
asHiveling = lens getter setter
 where
  getter :: Entity -> Maybe Hiveling'
  getter (Entity b (Hiveling d)) = Just (b, d)
  getter _                       = Nothing
  setter :: Entity -> Maybe Hiveling' -> Entity
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
offset2Direction p = find (direction2Offset `is` p) [minBound .. maxBound]

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

