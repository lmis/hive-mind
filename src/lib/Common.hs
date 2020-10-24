{-# LANGUAGE TemplateHaskell #-}
module Common where

import           System.IO                      ( Handle
                                                , hFlush
                                                , hPrint
                                                , hGetContents
                                                )
import           System.Process                 ( runInteractiveCommand
                                                , waitForProcess
                                                )
import           GHC.Float                      ( int2Double )
import           Data.List                      ( find )
import           Data.Maybe                     ( fromJust )
import           Control.Lens                   ( Prism'
                                                , (^.)
                                                , prism
                                                , makeLenses
                                                , makePrisms
                                                )

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



data EntityBase = EntityBase {
  _identifier :: !Int
 ,_position :: !Position
 ,_zIndex :: !Int
 ,_highlighted :: !Bool
} deriving (Eq, Show, Read)
makeLenses ''EntityBase

data Entity' a = Entity' {
  _base :: !EntityBase
 ,_details :: !a
} deriving (Eq, Show, Read)
makeLenses ''Entity'

data HivelingDetails = HivelingDetails {
  _lastDecision :: !Decision
 ,_hasNutrition :: !Bool
 ,_spreadsPheromones :: !Bool
} deriving (Eq, Show, Read)
makeLenses ''HivelingDetails

data EntityDetails = Hiveling' HivelingDetails
                   | Nutrition
                   | HiveEntrance
                   | Pheromone
                   | Obstacle deriving (Eq, Show, Read)
makePrisms ''EntityDetails


type Entity = Entity' EntityDetails
type Hiveling = Entity' HivelingDetails

data HivelingMindInput = HivelingMindInput {
  _closeEntities :: [Entity]
 ,_currentHiveling :: Hiveling
 ,_randomSeed :: Int
} deriving (Eq, Show, Read)
makeLenses ''HivelingMindInput

-- Traversals & Utils
asHiveling :: Prism' Entity Hiveling
asHiveling = prism collapse refine
 where
  collapse :: Hiveling -> Entity
  collapse h =
    Entity' { _base = h ^. base, _details = Hiveling' $ h ^. details }
  refine :: Entity -> Either Entity Hiveling
  refine (Entity' b (Hiveling' d)) = Right $ Entity' b d
  refine e                         = Left e


-- Direction
norm :: Position -> Double
norm (x, y) = sqrt . int2Double $ x * x + y * y

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
path p = case offset2Direction p of
  Just d -> [d]
  _ ->
    let dir    = closestDirection p
        remain = relativePosition (direction2Offset dir) p
    in  dir : path remain

-- Handles
hPrintFlush :: Show a => Handle -> a -> IO ()
hPrintFlush h x = do
  hPrint h x
  hFlush h

readCommand :: String -> IO String
readCommand cmd = do
  (_, hOut, _, hProc) <- runInteractiveCommand cmd
  v                   <- hGetContents hOut
  _                   <- waitForProcess hProc
  return v
