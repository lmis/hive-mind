{-# LANGUAGE TemplateHaskell #-}
module Client where

import           Control.Lens                   ( makeLenses )
import           Common                         ( Position
                                                , Entity(..)
                                                , EntityDetails(..)
                                                , Decision(..)
                                                )

data EntityBase = EntityBase {
  _position :: !Position
 ,_zIndex :: !Int
} deriving (Eq, Show, Read)
makeLenses ''EntityBase

data HivelingDetails = HivelingDetails {
  _hasNutrition :: !Bool
 ,_spreadsPheromones :: !Bool
 ,_recentDecisions :: ![Decision]
 ,_memory :: !String
} deriving (Eq, Show, Read)
makeLenses ''HivelingDetails

type EntityDetails' = EntityDetails HivelingDetails
type Entity' = Entity EntityBase EntityDetails'
type Hiveling' = Entity EntityBase HivelingDetails

data Input = Input {
  _closeEntities :: [Entity']
 ,_currentHiveling :: Hiveling'
 ,_randomSeed :: Int
} deriving (Eq, Show, Read)
makeLenses ''Input
