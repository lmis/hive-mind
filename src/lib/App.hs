{-# LANGUAGE TemplateHaskell #-}
module App where

import           Game                           ( GameState
                                                , startingState
                                                , doGameStep
                                                , entities
                                                , highlighted
                                                , position
                                                )
import           InteractiveCommand             ( InteractiveCommand
                                                , restart
                                                )
import           Common                         ( Position
                                                , base
                                                , readCommand
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( forever )
import           Control.Concurrent             ( ThreadId
                                                , threadDelay
                                                , forkIO
                                                )
import           Brick                          ( BrickEvent(..)
                                                , EventM
                                                , Next
                                                , continue
                                                , halt
                                                )
import qualified Graphics.Vty                  as V
import           Brick.BChan                    ( BChan
                                                , writeBChan
                                                )
import           Control.Lens                   ( (&)
                                                , (^.)
                                                , (%~)
                                                , (.~)
                                                , (+~)
                                                , (-~)
                                                , each
                                                , to
                                                , _1
                                                , _2
                                                , makeLenses
                                                )
import           Data.IORef                     ( IORef
                                                , readIORef
                                                , modifyIORef
                                                , writeIORef
                                                )

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

checkHotReloading :: BChan AppEvent -> IO ThreadId
checkHotReloading chan = forkIO $ forever $ do
  threadDelay 1000000
  writeBChan chan CheckHotReload

advanceGame :: IORef SpeedSettings -> BChan AppEvent -> IO ThreadId
advanceGame ref chan = forkIO $ forever $ do
  speedSettings' <- readIORef ref
  if speedSettings' ^. running
    then do
      threadDelay $ speedSettings' ^. delay . to (* 1000)
      writeBChan chan AdvanceGame
    else threadDelay $ 100 * 1000


setSpeed :: AppState -> Int -> EventM Name (Next AppState)
setSpeed s delay' = do
  _ <- liftIO $ modifyIORef (s ^. speedSettings) (delay .~ delay')
  continue s

handleEvent :: AppState
            -> BrickEvent Name AppEvent
            -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEsc        []       )) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'w') [])) =
  continue $ s & currentPage .~ World
handleEvent s (VtyEvent (V.EvKey (V.KChar 'l') [])) =
  continue $ s & currentPage .~ Logs
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [])) =
  continue $ s & currentPage .~ SelectedEntities
handleEvent s (VtyEvent (V.EvKey (V.KChar 'm') [])) =
  continue $ s & currentPage .~ Minimap
handleEvent s (VtyEvent (V.EvKey (V.KChar '?') [])) =
  continue $ s & currentPage .~ HelpPage
handleEvent s (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  _ <- liftIO $ modifyIORef (s ^. speedSettings) (running %~ not)
  continue s
handleEvent s (VtyEvent (V.EvKey V.KEnter      [])) = handleGameAdvance s
handleEvent s (VtyEvent (V.EvKey V.KUp [])) =
  continue $ s & renderArea %~ (_1 . _2 -~ 1) . (_2 . _2 -~ 1)
handleEvent s (VtyEvent (V.EvKey V.KDown [])) =
  continue $ s & renderArea %~ (_1 . _2 +~ 1) . (_2 . _2 +~ 1)
handleEvent s (VtyEvent (V.EvKey V.KLeft [])) =
  continue $ s & renderArea %~ (_1 . _1 -~ 1) . (_2 . _1 -~ 1)
handleEvent s (VtyEvent (V.EvKey V.KRight [])) =
  continue $ s & renderArea %~ (_1 . _1 +~ 1) . (_2 . _1 +~ 1)
handleEvent s (VtyEvent (V.EvKey (V.KChar '1') [])) = setSpeed s 300
handleEvent s (VtyEvent (V.EvKey (V.KChar '2') [])) = setSpeed s 100
handleEvent s (VtyEvent (V.EvKey (V.KChar '3') [])) = setSpeed s 50
handleEvent s (VtyEvent (V.EvKey (V.KChar '4') [])) = setSpeed s 10
handleEvent s (VtyEvent (V.EvKey (V.KChar 'v') [])) =
  continue $ s & hideUnseen %~ not
handleEvent s (MouseDown p V.BLeft _ _) =
  continue
    $  s
    &  gameState
    .  entities
    .  each
    .  base
    %~ (\e -> e & highlighted .~ (e ^. position == p))
handleEvent s (AppEvent CheckHotReload) = do
  currentVersion <- liftIO . readCommand $ s ^. getMindVersionCommand
  if currentVersion == (s ^. mindVersion)
    then continue s
    else do
      p <- liftIO restartMind
      continue $ s
        { _gameState       = startingState
        , _gameLogs = ["Version: " ++ currentVersion, "Restarting mind-command"]
                      ++ s
                      ^. gameLogs
        , _hiveMindProcess = p
        , _mindVersion     = currentVersion
        , _iteration       = 0
        }
 where
  restartMind = do
    speedSettings' <- readIORef (s ^. speedSettings)
    modifyIORef (s ^. speedSettings) (running .~ False)
    p <- restart $ s ^. hiveMindProcess
    writeIORef (s ^. speedSettings) speedSettings'
    return p
handleEvent s (AppEvent AdvanceGame) = handleGameAdvance s
handleEvent s _                      = continue s

handleGameAdvance :: AppState -> EventM Name (Next AppState)
handleGameAdvance s = do
  (nextState, logs) <- liftIO
    $ doGameStep (s ^. hiveMindProcess) (s ^. gameState)
  continue $ s & iteration +~ 1 & gameState .~ nextState & gameLogs %~ (logs ++)
