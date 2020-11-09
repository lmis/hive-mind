{-# LANGUAGE TemplateHaskell #-}
module Types where

import           InteractiveCommand             ( InteractiveCommand )
import           Game                           ( GameState )
import           Common                         ( Position )
import           Data.IORef                     ( IORef )
import           Control.Lens                   ( makeLenses )

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
