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
import           Common                         ( isNot
                                                , is
                                                , Entity'(..)
                                                , Entity
                                                , asHiveling
                                                , base
                                                , details
                                                , EntityBase(..)
                                                , identifier
                                                , position
                                                , zIndex
                                                , highlighted
                                                , EntityDetails(..)
                                                , HivelingDetails(..)
                                                , lastDecision
                                                , hasNutrition
                                                , spreadsPheromones
                                                , Hiveling
                                                , HivelingMindInput(..)
                                                , Position
                                                , Direction(..)
                                                , go
                                                , distance
                                                , relativePosition
                                                , Decision(..)
                                                , DecisionType(..)
                                                , hPrintFlush
                                                , readCommand
                                                )
import           System.Process                 ( ProcessHandle
                                                , runInteractiveCommand
                                                , cleanupProcess
                                                )
import           System.IO                      ( Handle
                                                , stderr
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
import           Text.Pretty.Simple             ( pShowNoColor )
import           Data.Text.Lazy                 ( Text
                                                , toStrict
                                                )
import           Data.IORef                     ( IORef
                                                , readIORef
                                                , modifyIORef
                                                , newIORef
                                                , writeIORef
                                                )
import           Safe.Foldable                  ( maximumByMay )
import           Data.Ord                       ( comparing )
import           Data.List                      ( find
                                                , maximumBy
                                                , sortOn
                                                , foldl'
                                                )
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                , fromListWith
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
                                                , Widget
                                                , customMain
                                                , neverShowCursor
                                                , continue
                                                , halt
                                                , clickable
                                                , attrMap
                                                , (<+>)
                                                , (<=>)
                                                , vBox
                                                , hBox
                                                , withBorderStyle
                                                , str
                                                , txt
                                                , padLeftRight
                                                )
import qualified Brick.Widgets.Center          as C
import qualified Graphics.Vty                  as V
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import           Brick.BChan                    ( BChan
                                                , newBChan
                                                , writeBChan
                                                )
import           System.Random                  ( StdGen
                                                , mkStdGen
                                                , next
                                                )
import           Lens.Micro                     ( Traversal'
                                                , (&)
                                                , (^.)
                                                , (^..)
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
import           Lens.Micro.Platform            ( makeLenses )


data GameState = GameState {
  _entities :: [Entity]
 ,_nextId :: !Int
 ,_score :: !Int
 ,_randomGen :: StdGen
} deriving (Show)
makeLenses ''GameState

data InteractiveCommand = InteractiveCommand {
  _command :: !String,
  _startupTimeout :: !Int,
  _hIn :: Handle,
  _hOut :: Handle,
  _hErr :: Handle,
  _hProc :: ProcessHandle
}
makeLenses ''InteractiveCommand

type Name = Position
data AppEvent = AdvanceGame | CheckHotReload deriving (Eq, Show, Ord)
data AppState = AppState {
  _gameState :: !GameState
 ,_hiveMindProcess :: !InteractiveCommand
 ,_mindVersion :: !String
 ,_getMindVersionCommand :: !String
 ,_running :: IORef (Bool, Int)
 ,_hideUnseen :: !Bool
 ,_iteration :: !Int
}
makeLenses ''AppState

-- Game init & advancing
addEntity :: Int -> EntityDetails -> Position -> GameState -> GameState
addEntity _zIndex _details _position state =
  state & nextId +~ 1 & entities %~ (new :)
 where
  new = Entity'
    { _base = EntityBase { _identifier  = state ^. nextId
                         , _highlighted = False
                         , ..
                         }
    , ..
    }

startingState :: GameState
startingState =
  foldr
      (uncurry $ addEntity 0)
      GameState { _entities  = []
                , _nextId    = 0
                , _score     = 0
                , _randomGen = mkStdGen 42
                }
    $  (   ( Hiveling' $ HivelingDetails { _lastDecision = Decision Move Center
                                         , _hasNutrition = False
                                         , _spreadsPheromones = False
                                         }
           ,
           )
       <$> hivelingPositions
       )
    ++ ((HiveEntrance, ) <$> entrances)
    ++ ((Obstacle, ) <$> topAndBottom)
    ++ ((Obstacle, ) <$> sides)
    ++ ((Nutrition, ) <$> nutrition)
 where
  hivelingPositions = [(1, 4), (-3, 12), (0, -6), (2, 2)]
  entrances         = (,) <$> [-5, 5] <*> [-5, 5]
  topAndBottom      = (,) <$> [-9 .. 9] <*> [-20, -19, 19, 20]
  sides             = (,) <$> [-10, 10] <*> [-20 .. 20]
  nutrition         = (,) <$> [-5 .. 5] <*> [-15, -14, 0, 14, 15]

sees :: Hiveling -> Position -> Bool
sees h p = distance p (h ^. base . position) < 6

doGameStep :: InteractiveCommand -> GameState -> IO GameState
doGameStep proc state = do
  stdGenRef             <- newIORef (state ^. randomGen)
  -- TODO: Shuffle
  hivelingsWithDecision <- mapM (takeDecision stdGenRef) hivelings
  gen                   <- readIORef stdGenRef
  return $ foldl' applyDecision (state & randomGen .~ gen) hivelingsWithDecision
 where
  hivelings :: [Hiveling]
  hivelings = state ^.. entities . each . asHiveling . _Just
  takeDecision :: IORef StdGen -> Hiveling -> IO (Hiveling, Decision)
  takeDecision stdGenRef hiveling = do
    g <- readIORef stdGenRef
    let (r, g') = next g
    decision <- getHiveMindDecision
      proc
      HivelingMindInput
        { _closeEntities   =
          state
          ^.. entities
          .   each
          .   filtered
                ((^. base . identifier) `isNot` (hiveling ^. base . identifier))
          .   filtered ((hiveling `sees`) . (^. base . position))
          &   each
          %~  forHivelingMind hiveling
        , _currentHiveling = hiveling
        , _randomSeed      = r
        }
    writeIORef stdGenRef g'
    return (hiveling, decision)
  forHivelingMind :: Hiveling -> Entity -> Entity
  forHivelingMind hiveling e =
    e
      &  base
      .  position
      %~ relativePosition (hiveling ^. base . position)
      &
      -- Hivelings don't know about ids
         base
      .  identifier
      .~ -1

applyDecision :: GameState -> (Hiveling, Decision) -> GameState
applyDecision state (h, decision@(Decision t direction)) =
  (case t of
      Move   -> case topEntityAtTarget of
        Just (Entity' _ Obstacle     ) -> state & score -~ 1
        Just (Entity' _ (Hiveling' _)) -> state
        Just (Entity' b' _) ->
          state
            &  hiveling
            .  base
            %~ (position .~ targetPos)
            .  (zIndex .~ (b' ^. zIndex) + 1)
        Nothing ->
          state & hiveling . base %~ (position .~ targetPos) . (zIndex .~ 0)
      Pickup -> case topEntityAtTarget of
        Just (Entity' _ Nutrition) -> if h ^. details . hasNutrition
          then state
          else
            state
            &  hiveling
            .  details
            .  hasNutrition
            .~ True
            &  entities
            %~ filter (Just `isNot` topEntityAtTarget)
        _                          -> state
      Drop   -> case topEntityAtTarget of
        Just (Entity' _ HiveEntrance) -> if h ^. details . hasNutrition
          then state & hiveling . details . hasNutrition .~ False & score +~ 10
          else state
        Just _                        -> state
        Nothing ->
          state
            &  score
            -~ 100 -- No food waste!
            &  hiveling
            .  details
            .  hasNutrition
            .~ False
    )
    &  hiveling
    .  details
    .  lastDecision
    .~ decision
 where
  targetPos :: Position
  targetPos = (h ^. base . position) `go` direction
  topEntityAtTarget :: Maybe Entity
  topEntityAtTarget =
    maximumByMay (comparing (^. base . zIndex))
      $   state
      ^.. entities
      .   each
      .   filtered ((^. base . position) `is` targetPos)
      .   filtered ((^. base . identifier) `isNot` (h ^. base . identifier))
  hiveling :: Traversal' GameState Hiveling
  hiveling =
    entities
      . each
      . filtered ((^. base . identifier) `is` (h ^. base . identifier))
      . asHiveling
      . _Just


getHiveMindDecision :: InteractiveCommand -> HivelingMindInput -> IO Decision
getHiveMindDecision cmd input = do
  hPrintFlush (cmd ^. hIn) input
  ready <- hWaitForInput (cmd ^. hOut) 100
  if ready
    then do
      output <- hGetLine (cmd ^. hOut)
      return $ read output
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
  running'         <- newIORef (False, 100)
  chan             <- newBChan 10
  _                <- advanceGame running' chan
  _                <- checkHotReloading chan

  initialVty       <- buildVty
  void $ customMain
    initialVty
    buildVty
    (Just chan)
    app
    AppState { _gameState             = startingState
             , _iteration             = 0
             , _hideUnseen            = False
             , _hiveMindProcess       = hiveMindProcess'
             , _mindVersion           = mindVersion'
             , _getMindVersionCommand = getMindVersionCommand'
             , _running               = running'
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

advanceGame :: IORef (Bool, Int) -> BChan AppEvent -> IO ThreadId
advanceGame ref chan = forkIO $ forever $ do
  runs <- readIORef ref
  case runs of
    (True, delay) -> do
      threadDelay $ delay * 1000
      writeBChan chan AdvanceGame
    _             -> threadDelay $ 100 * 1000


setSpeed :: AppState -> Int -> EventM Name (Next AppState)
setSpeed s delay = do
  _ <- liftIO $ modifyIORef (s ^. running) (_2 .~ delay)
  continue s

handleEvent :: AppState
            -> BrickEvent Name AppEvent
            -> EventM Name (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KEsc        []       )) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar ' ') []       )) = do
  _ <- liftIO $ modifyIORef (s ^. running) (_1 %~ not)
  continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar '1') []       )) = setSpeed s 300
handleEvent s (VtyEvent (V.EvKey (V.KChar '2') []       )) = setSpeed s 100
handleEvent s (VtyEvent (V.EvKey (V.KChar '3') []       )) = setSpeed s 50
handleEvent s (VtyEvent (V.EvKey (V.KChar '4') []       )) = setSpeed s 10
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
      continue $ s { _gameState       = startingState
                   , _hiveMindProcess = p
                   , _mindVersion     = currentVersion
                   , _iteration       = 0
                   }
 where
  restartMind = do
    runs <- readIORef (s ^. running)
    modifyIORef (s ^. running) (_1 .~ False)
    p <- restart $ s ^. hiveMindProcess
    writeIORef (s ^. running) runs
    return p
handleEvent s (AppEvent AdvanceGame) = do
  nextState <- liftIO $ doGameStep (s ^. hiveMindProcess) (s ^. gameState)
  continue $ s & iteration +~ 1 & gameState .~ nextState
handleEvent s _                      = continue s

-- Rendering
drawGameState :: AppState -> Widget Name
drawGameState state =
  vBox [ hBox [ renderPosition (x, y) | x <- [-10 .. 10] ] | y <- [-20 .. 20] ]
 where
  renderPosition :: Position -> Widget Name
  renderPosition p = clickable p . str . obscureInvisible p $ maybe
    "  "
    (^. details . to render)
    (pointsOfInterest !? p)
  pointsOfInterest :: Map Position Entity
  pointsOfInterest =
    fromListWith
        (\old new -> maximumBy (comparing (^. base . zIndex)) [old, new])
      $   state
      ^.. gameState
      .   entities
      .   each
      .   to (\e -> (e ^. base . position, e))
  highlights :: [Position]
  highlights =
    state
      ^.. gameState
      .   entities
      .   each
      .   base
      .   filtered (^. highlighted)
      .   position
  hivelings :: [Hiveling]
  hivelings = state ^.. gameState . entities . each . asHiveling . _Just
  noHivelingSees :: Position -> Bool
  noHivelingSees p = not $ any (`sees` p) hivelings
  noHighlightClose :: Position -> Bool
  noHighlightClose p = all ((> 2.5) . distance p) highlights
  obscureInvisible :: Position -> String -> String
  obscureInvisible p s
    | state ^. hideUnseen && noHighlightClose p && noHivelingSees p = "??"
    | otherwise = s
  render :: EntityDetails -> String
  render t = case t of
    Nutrition    -> "**"
    HiveEntrance -> "{}"
    Pheromone    -> ".~"
    Obstacle     -> "XX"
    Hiveling' h | h ^. hasNutrition && h ^. spreadsPheromones -> "U*"
                | h ^. hasNutrition      -> "J*"
                | h ^. spreadsPheromones -> "U="
                | otherwise              -> "J="

drawUI :: AppState -> [Widget Name]
drawUI s =
  [ C.hCenter
      .   labeledBorder "Hive Mind"
      $   (  C.hCenter
          .  labeledBorder "Score"
          $  padLeftRight 5
          .  str
          .  show
          $  s
          ^. gameState
          .  score
          )
      <=> (C.hCenter (labeledBorder "World" (drawGameState s)) <+> C.hCenter
            (labeledBorder "Selected" $ if null highlights
              then padLeftRight 10 $ str "Nothing selected"
              else vBox (highlightBox <$> highlights)
            )
          )
  ]
 where
  highlights :: [Entity]
  highlights = sortOn
    (^. base . zIndex . to negate)
    (s ^.. gameState . entities . each . filtered (^. base . highlighted))
  highlightBox :: Entity -> Widget Name
  highlightBox e =
    C.hCenter
      $  labeledBorder (e ^. base . position . to show)
      .  txt
      .  toStrict
      .  info
      $  e
      ^. details
  info :: EntityDetails -> Text
  info (Hiveling' d) = pShowNoColor d
  info d             = pShowNoColor d


labeledBorder :: String -> Widget a -> Widget a
labeledBorder label =
  withBorderStyle BS.unicodeBold
    . B.borderWithLabel (padLeftRight 1 $ str label)
