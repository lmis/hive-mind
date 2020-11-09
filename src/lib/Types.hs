{-# LANGUAGE TemplateHaskell #-}
module Types where

import           InteractiveCommand             ( InteractiveCommand )
import           Common                         ( Entity(..)
                                                , base
                                                , details
                                                , distance
                                                , EntityDetails(..)
                                                , Position
                                                , Rotation(..)
                                                , Decision(..)
                                                )
import           Data.IORef                     ( IORef )
import           System.Random                  ( StdGen )
import           Control.Lens                   ( Prism'
                                                , prism
                                                , (^.)
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


data SpeedSettings = SpeedSettings {
  _running :: !Bool
 ,_delay :: !Int
}
makeLenses ''SpeedSettings

data Page = HelpPage | SelectedEntities | Minimap | World | Logs deriving (Eq, Show, Ord)

type Name = Position
data AppEvent = AdvanceGame | CheckHotReload deriving (Eq, Show, Ord)
data AppState = AppState {
  _gameState :: !GameState
 ,_gameLogs :: [String]
 ,_renderArea :: !(Position, Position)
 ,_hiveMindProcess :: !InteractiveCommand
 ,_mindVersion :: !String
 ,_getMindVersionCommand :: !String
 ,_speedSettings :: IORef SpeedSettings
 ,_hideUnseen :: !Bool
 ,_currentPage :: !Page
 ,_iteration :: !Int
}
makeLenses ''AppState

-- Traversals & Utils
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
