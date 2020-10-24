{-# LANGUAGE TemplateHaskell #-}
module Client where

import           Control.Lens                   ( makeLenses )
import           Common                         ( Position
                                                , Entity(..)
                                                , EntityDetails(..)
                                                , HivelingDetails
                                                )

data EntityBase = EntityBase {
  _position :: !Position
 ,_zIndex :: !Int
} deriving (Eq, Show, Read)
makeLenses ''EntityBase

type Entity' = Entity EntityBase EntityDetails
type Hiveling' = Entity EntityBase HivelingDetails

data Input = Input {
  _closeEntities :: [Entity']
 ,_currentHiveling :: Hiveling'
 ,_randomSeed :: Int
} deriving (Eq, Show, Read)
makeLenses ''Input
