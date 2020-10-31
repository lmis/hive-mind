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
import           Control.Lens                   ( Prism'
                                                , (^.)
                                                , prism
                                                , makeLenses
                                                , makePrisms
                                                )

type Position = (Int, Int)

data Rotation = None
              | Clockwise
              | Back
              | Counterclockwise deriving (Eq, Show, Read, Ord, Enum, Bounded)

data Decision = Wait
              | Turn Rotation
              | Move
              | Pickup
              | Drop deriving (Eq, Show, Read)

data EntityBase = EntityBase {
  _identifier :: !Int
 ,_position :: !Position
 ,_zIndex :: !Int
 ,_highlighted :: !Bool
} deriving (Eq, Show, Read)
makeLenses ''EntityBase

data Entity b d = Entity {
  _base :: !b
 ,_details :: !d
} deriving (Eq, Show, Read)
makeLenses ''Entity

data HivelingDetails = HivelingDetails {
  _lastDecision :: !Decision
 ,_hasNutrition :: !Bool
 ,_spreadsPheromones :: !Bool
 ,_orientation :: !Rotation -- Rotation w.r.t North
} deriving (Eq, Show, Read)
makeLenses ''HivelingDetails

data EntityDetails = Hiveling HivelingDetails
                   | Nutrition
                   | HiveEntrance
                   | Pheromone
                   | Obstacle deriving (Eq, Show, Read)
makePrisms ''EntityDetails

type Entity' b = Entity b EntityDetails
type Hiveling' b = Entity b HivelingDetails
-- Traversals & Utils
asHiveling :: Prism' (Entity' b) (Hiveling' b)
asHiveling = prism collapse refine
 where
  collapse :: Hiveling' b -> Entity' b
  collapse h = Entity { _base = h ^. base, _details = Hiveling $ h ^. details }
  refine :: Entity' b -> Either (Entity' b) (Hiveling' b)
  refine (Entity b (Hiveling d)) = Right $ Entity b d
  refine e                       = Left e


-- Position and Rotation
addRotations :: Rotation -> Rotation -> Rotation
addRotations r1 r2 = toEnum $ (fromEnum r1 + fromEnum r2) `mod` 4

norm :: Position -> Double
norm (x, y) = sqrt . int2Double $ x * x + y * y

relativePosition :: Position -> Position -> Position
relativePosition (ox, oy) (x, y) = (x - ox, y - oy)


distance :: Position -> Position -> Double
distance p q = norm $ relativePosition p q

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
