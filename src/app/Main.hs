module Main
  ( main
  )
where

import           DemoMind                       ( runDemo )
import           Types                          ( AppEvent(..)
                                                , Name
                                                , AppState(..)
                                                , SpeedSettings(..)
                                                , hideUnseen
                                                , running
                                                , delay
                                                , speedSettings
                                                , currentPage
                                                , renderArea
                                                , gameState
                                                , getMindVersionCommand
                                                , mindVersion
                                                , gameLogs
                                                , hiveMindProcess
                                                , iteration
                                                , Page(..)
                                                )
import           Game                           ( startingState
                                                , doGameStep
                                                , entities
                                                , highlighted
                                                , position
                                                )
import           InteractiveCommand             ( start
                                                , restart
                                                )
import           Render                         ( drawUI )
import           Common                         ( base
                                                , readCommand
                                                )
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                , exitSuccess
                                                )
import           System.Console.GetOpt          ( OptDescr(..)
                                                , ArgDescr(..)
                                                , getOpt
                                                , usageInfo
                                                , ArgOrder(..)
                                                )
import           System.Environment             ( getArgs )
import           Data.IORef                     ( IORef
                                                , readIORef
                                                , modifyIORef
                                                , newIORef
                                                , writeIORef
                                                )
import           Data.List                      ( find )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Concurrent             ( ThreadId
                                                , threadDelay
                                                , forkIO
                                                )
import           Brick                          ( App(..)
                                                , BrickEvent(..)
                                                , EventM
                                                , Next
                                                , customMain
                                                , neverShowCursor
                                                , continue
                                                , halt
                                                , attrMap
                                                )
import qualified Graphics.Vty                  as V
import           Brick.BChan                    ( BChan
                                                , newBChan
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
                                                )

-- App plumbing
data Flag = Help | MindCommand String | GetMindVersion String | RunAsDemoMind deriving (Eq, Show)
flags :: [OptDescr Flag]
flags =
  [ Option ['d']
           ["demo-mind"]
           (NoArg RunAsDemoMind)
           "Run program as demo hiveling mind"
  , Option ['m'] ["mind-command"] (ReqArg MindCommand "CMD") $ unlines
    [ "Command to process HivelingMindInput from stdin and writing Decision to stdout."
    , "Must print one line to stdout on startup and then continuously read from stdin and write to stdout."
    ]
  , Option
    ['v']
    ["get-mind-version"]
    (ReqArg GetMindVersion "CMD")
    "Command to get version of `mind-command`. If changed, `mind-command` is restarted."
  , Option ['h'] ["help"] (NoArg Help) "Print this help message"
  ]

main :: IO ()
main = do
  argv <- getArgs
  case getOpt Permute flags argv of
    (args, _, []    ) -> do
      if Help `elem` args
        then do
          hPutStrLn stderr (usageInfo header flags)
          exitSuccess
        else if RunAsDemoMind `elem` args
          then runDemo
          else case (find isMindCommand args, find isGetMindVersion args) of
            (Just (MindCommand r), Just (GetMindVersion g)) ->
              runApp (r, g) args
            (Nothing, Just (GetMindVersion _)) ->
              failure ["Missing mind-command"]
            (Just (MindCommand _), Nothing) ->
              failure ["Missing get-mind-version"]
            _ -> failure ["Missing mind-command and get-mind-version"]
    (_   , _, errors) -> failure errors
 where
  isMindCommand :: Flag -> Bool
  isMindCommand (MindCommand _) = True
  isMindCommand _               = False
  isGetMindVersion :: Flag -> Bool
  isGetMindVersion (GetMindVersion _) = True
  isGetMindVersion _                  = False
  header :: String
  header = "Usage"
  failure :: [String] -> IO ()
  failure errors = do
    hPutStrLn stderr (unlines $ errors ++ [usageInfo header flags])
    exitWith (ExitFailure 1)

runApp :: (String, String) -> [Flag] -> IO ()
runApp (mindCommand, getMindVersionCommand') _ = do
  hiveMindProcess' <- start 3000 mindCommand
  mindVersion'     <- readCommand getMindVersionCommand'

  -- channel to inject events into main loop
  speedSettings' <- newIORef $ SpeedSettings { _running = False, _delay = 100 }
  chan             <- newBChan 10
  _                <- advanceGame speedSettings' chan
  _                <- checkHotReloading chan

  initialVty       <- buildVty
  void $ customMain
    initialVty
    buildVty
    (Just chan)
    app
    AppState
      { _gameState             = startingState
      , _gameLogs              = [ "Version: " ++ mindVersion'
                                 , "Started mind-command: " ++ mindCommand
                                 ]
      , _renderArea            = ((-10, -10), (10, 10))
      , _iteration             = 0
      , _hideUnseen            = False
      , _currentPage           = World
      , _hiveMindProcess       = hiveMindProcess'
      , _mindVersion           = mindVersion'
      , _getMindVersionCommand = getMindVersionCommand'
      , _speedSettings         = speedSettings'
      }
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
          , appAttrMap      = const $ attrMap V.defAttr []
          }

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
