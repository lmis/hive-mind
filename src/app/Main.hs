{-# LANGUAGE
   RankNTypes
  ,TupleSections
  ,RecordWildCards
  ,NamedFieldPuns
  ,TemplateHaskell #-}
module Main
  ( main
  )
where

import           DemoMind                       ( runDemo )
import           Types                          ( AppEvent(..)
                                                , Name
                                                , AppState(..)
                                                , GameState(..)
                                                , Entity'
                                                , EntityBase(..)
                                                , EntityDetails'
                                                , asHiveling
                                                , HivelingDetails(..)
                                                , Hiveling'
                                                , InteractiveCommand(..)
                                                , sees
                                                , SpeedSettings(..)
                                                , nextId
                                                , entities
                                                , position
                                                , randomGen
                                                , identifier
                                                , orientation
                                                , memory
                                                , score
                                                , zIndex
                                                , hasNutrition
                                                , spreadsPheromones
                                                , recentDecisions
                                                , hIn
                                                , hOut
                                                , hErr
                                                , hProc
                                                , startupTimeout
                                                , command
                                                , running
                                                , delay
                                                , speedSettings
                                                , Page(..)
                                                , currentPage
                                                , renderArea
                                                , hideUnseen
                                                , gameState
                                                , highlighted
                                                , getMindVersionCommand
                                                , mindVersion
                                                , gameLogs
                                                , hiveMindProcess
                                                , iteration
                                                , hideUnseen
                                                )
import           Render                         ( drawUI )
import           Common                         ( Entity(..)
                                                , base
                                                , details
                                                , EntityDetails(..)
                                                , Position
                                                , Rotation(..)
                                                , addRotations
                                                , relativePosition
                                                , Decision(..)
                                                , hPrintFlush
                                                , generatorList
                                                , readCommand
                                                )
import qualified Client                         ( Input(..)
                                                , EntityBase(..)
                                                , EntityDetails'
                                                , HivelingDetails(..)
                                                )
import           GHC.Float                      ( int2Double )
import           System.Process                 ( runInteractiveCommand
                                                , cleanupProcess
                                                )
import           System.IO                      ( stderr
                                                , hSetBinaryMode
                                                , hPutStrLn
                                                , hGetLine
                                                , hWaitForInput
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
import           System.Random.Shuffle          ( shuffle' )
import           Text.Pretty.Simple             ( pShowNoColor )
import           Data.Text.Lazy                 ( unpack )
import           Data.IORef                     ( IORef
                                                , readIORef
                                                , modifyIORef
                                                , newIORef
                                                , writeIORef
                                                )
import           Safe.Foldable                  ( maximumByMay )
import           Data.Ord                       ( comparing )
import           Data.List                      ( find
                                                , foldl'
                                                )
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
import           System.Random                  ( StdGen
                                                , mkStdGen
                                                , next
                                                )
import           Control.Lens                   ( ASetter'
                                                , (&)
                                                , (^.)
                                                , (^..)
                                                , (^?)
                                                , (%~)
                                                , (.~)
                                                , (+~)
                                                , (-~)
                                                , each
                                                , to
                                                , filtered
                                                , _Just
                                                , _1
                                                , _2
                                                )

-- Game init & advancing
addEntity :: (EntityDetails', Position) -> GameState -> GameState
addEntity (details', position') state =
  state & nextId +~ 1 & entities %~ (new :)
 where
  zIndex' =
    length
      $   state
      ^.. entities
      .   each
      .   filtered (\e -> (e ^. base . position) == position')
  new = Entity
    { _base    = EntityBase { _identifier  = state ^. nextId
                            , _highlighted = False
                            , _zIndex      = zIndex'
                            , _position    = position'
                            }
    , _details = details'
    }

startingState :: GameState
startingState =
  foldr
      addEntity
      GameState { _entities  = []
                , _nextId    = 0
                , _score     = 0
                , _randomGen = mkStdGen 42
                }
    $  ((Hiveling defaultHivelingDetails, ) <$> hivelingPositions)
    ++ ((HiveEntrance, ) <$> entrances)
    ++ ((Obstacle, ) <$> topAndBottom)
    ++ ((Obstacle, ) <$> sides)
    ++ ((Nutrition, ) <$> nutrition)
 where
  defaultHivelingDetails = HivelingDetails { _recentDecisions   = []
                                           , _memory            = ""
                                           , _hasNutrition      = False
                                           , _spreadsPheromones = False
                                           , _orientation       = None
                                           }
  hivelingPositions      = [(1, 4), (-3, 12), (0, -6), (2, 2)]
  entrances              = (,) <$> [-5, 5] <*> [-5, 5]
  topAndBottom           = (,) <$> [-9 .. 9] <*> [-16, 16]
  sides                  = (,) <$> [-10, 10] <*> [-16 .. 16]
  nutrition              = (,) <$> [-5 .. 5] <*> [-15, -14, 0, 14, 15]

doGameStep :: InteractiveCommand -> GameState -> IO (GameState, [String])
doGameStep proc state = do
  let (g : g' : generators) = generatorList $ state ^. randomGen
  let shuffledHivelings     = shuffle' hivelings (length hivelings) g

  decisionsWithMetadata <- mapM takeDecision $ zip generators shuffledHivelings

  return
    $ foldl' applyDecision (state & randomGen .~ g', []) decisionsWithMetadata
 where
  hivelings :: [Hiveling']
  hivelings = state ^.. entities . each . asHiveling
  takeDecision :: (StdGen, Hiveling') -> IO (Decision, Hiveling', Client.Input)
  takeDecision (g, hiveling) = do
    let
      (r, _) = next g
      input  = Client.Input
        { _closeEntities   =
          state
          ^.. entities
          .   each
          .   filtered
                (\e ->
                  (e ^. base . identifier) /= (hiveling ^. base . identifier)
                )
          .   filtered (\e -> hiveling `sees` (e ^. base . position))
          &   each
          %~  forHivelingMind hiveling
        , _currentHiveling = hivelingWithClientTypes hiveling
        , _randomSeed      = r
        }
    decision <- getHiveMindDecision proc input
    return (decision, hiveling, input)
  hivelingWithClientTypes :: Hiveling'
                          -> Entity Client.EntityBase Client.HivelingDetails
  hivelingWithClientTypes h =
    h
      &  base
      %~ (\EntityBase {..} -> Client.EntityBase { .. })
      &  details
      %~ \HivelingDetails {..} -> Client.HivelingDetails { .. }
  forHivelingMind :: Hiveling'
                  -> Entity EntityBase EntityDetails'
                  -> Entity Client.EntityBase Client.EntityDetails'
  forHivelingMind h e =
    e
      &  base
      %~ (\EntityBase {..} -> Client.EntityBase
           { _position =
             inverseRotatePosition (h ^. details . orientation)
               $ relativePosition (h ^. base . position) (e ^. base . position)
           , ..
           }
         )
      &  details
      .~ case e ^. details of
           Hiveling HivelingDetails {..} ->
             Hiveling Client.HivelingDetails { .. }
           Nutrition    -> Nutrition
           Obstacle     -> Obstacle
           HiveEntrance -> HiveEntrance
           Pheromone    -> Pheromone
  inverseRotatePosition :: Rotation -> Position -> Position
  inverseRotatePosition None             p      = p
  inverseRotatePosition Clockwise        (x, y) = (-y, x)
  inverseRotatePosition Counterclockwise (x, y) = (y, -x)
  inverseRotatePosition Back             (x, y) = (-x, -y)

-- TODO state monad
applyDecision :: (GameState, [String])
              -> (Decision, Hiveling', Client.Input)
              -> (GameState, [String])
applyDecision (state, logs) (decision, h, input) =
  ( (case decision of
      Remember128Characters msg ->
        state
          &  hiveling
          .  details
          .  memory
          .~ msg
          &  score
          -~ round (int2Double (length msg) / 20.0)
      Turn None -> state & score -~ 1
      Turn rotation ->
        state & hiveling . details . orientation %~ addRotations rotation
      Move   -> case topEntityAtTarget of
        Just (Entity _ Obstacle    ) -> state & score -~ 2
        Just (Entity _ (Hiveling _)) -> state
        Just (Entity b' _) ->
          state
            &  hiveling
            .  base
            %~ (position .~ targetPos)
            .  (zIndex .~ (b' ^. zIndex) + 1)
        Nothing ->
          state & hiveling . base %~ (position .~ targetPos) . (zIndex .~ 0)
      Pickup -> case topEntityAtTarget of
        Just (Entity _ Nutrition) -> if h ^. details . hasNutrition
          then state
          else
            state
            &  hiveling
            .  details
            .  hasNutrition
            .~ True
            &  entities
            %~ filter
                 (\e ->
                   Just (e ^. base . identifier)
                     /= topEntityAtTarget
                     ^? _Just
                     .  base
                     .  identifier
                 )
        _                         -> state
      Drop   -> case topEntityAtTarget of
        Just (Entity _ HiveEntrance) -> if h ^. details . hasNutrition
          then state & hiveling . details . hasNutrition .~ False & score +~ 15
          else state
        Just _                       -> state
        Nothing                      -> if h ^. details . hasNutrition
          then
            state
            &  hiveling
            .  details
            .  hasNutrition
            .~ False
            &  addEntity (Nutrition, targetPos)
          else state
    )
    &  (if h ^. details . spreadsPheromones
         then addEntity (Pheromone, h ^. base . position)
         else id
       )
    &  hiveling
    .  details
    .  recentDecisions
    %~ (\ds -> decision : take 2 ds)
  , [ "< " ++ (unpack . pShowNoColor $ decision)
    , "> " ++ (unpack . pShowNoColor $ input)
    ]
    ++ logs
  )
 where
  targetPos :: Position
  targetPos =
    let (x, y) = h ^. base . position
    in  case h ^. details . orientation of
          None             -> (x, y + 1)
          Clockwise        -> (x + 1, y)
          Back             -> (x, y - 1)
          Counterclockwise -> (x - 1, y)
  topEntityAtTarget :: Maybe Entity'
  topEntityAtTarget =
    maximumByMay (comparing (^. base . zIndex))
      $   state
      ^.. entities
      .   each
      .   filtered (\e -> (e ^. base . position) == targetPos)
      .   filtered (\e -> (e ^. base . identifier) /= (h ^. base . identifier))
  -- This could be a Traversal' but since you can get the hiveling through `h`,
  -- this should stay an ASetter'
  hiveling :: ASetter' GameState Hiveling'
  hiveling =
    entities
      . each
      . filtered (\e -> (e ^. base . identifier) == (h ^. base . identifier))
      . asHiveling


getHiveMindDecision :: InteractiveCommand -> Client.Input -> IO Decision
getHiveMindDecision cmd input = do
  hPrintFlush (cmd ^. hIn) input
  ready <- hWaitForInput (cmd ^. hOut) 100
  if ready
    then do
      output <- hGetLine (cmd ^. hOut)
      let parsed = read output
      return $ case parsed of
        Remember128Characters msg -> if length msg > 128
          then error "Attempting to remember too large messge"
          else parsed
        _                         -> parsed
    else error "mind-command decision time-out"


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

start :: Int -> String -> IO InteractiveCommand
start startupTimeout' command' = do
  (hIn', hOut', hErr', hProc') <- runInteractiveCommand command'
  _                            <- hSetBinaryMode hIn' False
  _                            <- hSetBinaryMode hOut' False
  ready                        <- hWaitForInput hOut' startupTimeout'
  if ready then void $ hGetLine hOut' else error $ command' ++ " start time-out"

  return InteractiveCommand { _command        = command'
                            , _startupTimeout = startupTimeout'
                            , _hIn            = hIn'
                            , _hOut           = hOut'
                            , _hErr           = hErr'
                            , _hProc          = hProc'
                            }

kill :: InteractiveCommand -> IO ()
kill cmd = cleanupProcess
  (Just $ cmd ^. hIn, Just $ cmd ^. hOut, Just $ cmd ^. hErr, cmd ^. hProc)

restart :: InteractiveCommand -> IO InteractiveCommand
restart p = do
  kill p
  start (p ^. startupTimeout) $ p ^. command

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
