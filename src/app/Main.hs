module Main
  ( main
  )
where

import           DemoMind                       ( runDemo )
import           App                            ( AppState(..)
                                                , SpeedSettings(..)
                                                , Page(..)
                                                , advanceGame
                                                , checkHotReloading
                                                , handleEvent
                                                )
import           Game                           ( startingState )
import           InteractiveCommand             ( start )
import           Render                         ( drawUI )
import           Common                         ( readCommand )
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
import           Data.IORef                     ( newIORef )
import           Data.List                      ( find )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( void )
import           Brick                          ( App(..)
                                                , customMain
                                                , neverShowCursor
                                                , attrMap
                                                )
import qualified Graphics.Vty                  as V
import           Brick.BChan                    ( newBChan )

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
    App { appDraw         = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent  = handleEvent
        , appStartEvent   = return
        , appAttrMap      = const $ attrMap V.defAttr []
        }
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
