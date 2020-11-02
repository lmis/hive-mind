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
import           Common                         ( Entity(..)
                                                , base
                                                , details
                                                , EntityDetails(..)
                                                , Position
                                                , Rotation(..)
                                                , addRotations
                                                , distance
                                                , relativePosition
                                                , Decision(..)
                                                , hPrintFlush
                                                , readCommand
                                                )
import qualified Client                         ( Input(..)
                                                , EntityBase(..)
                                                , EntityDetails'
                                                , HivelingDetails(..)
                                                )
import           GHC.Float                      ( int2Double )
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
import           System.Random.Shuffle          ( shuffle' )
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
import           Data.Ord                       ( Down(..)
                                                , comparing
                                                )
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
                                                , split
                                                , mkStdGen
                                                , next
                                                )
import           Control.Lens                   ( ASetter'
                                                , Prism'
                                                , prism
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

data InteractiveCommand = InteractiveCommand {
  _command :: !String
 ,_startupTimeout :: !Int
 ,_hIn :: Handle
 ,_hOut :: Handle
 ,_hErr :: Handle
 ,_hProc :: ProcessHandle
}
makeLenses ''InteractiveCommand

data SpeedSettings = SpeedSettings {
  _running :: !Bool
 ,_delay :: !Int
}
makeLenses ''SpeedSettings

data Page = HelpPage | SelectedEntities | Minimap | World deriving (Eq, Show, Ord)

type Name = Position
data AppEvent = AdvanceGame | CheckHotReload deriving (Eq, Show, Ord)
data AppState = AppState {
  _gameState :: !GameState
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

sees :: Entity EntityBase d -> Position -> Bool
sees h p = distance p (h ^. base . position) < 6

doGameStep :: InteractiveCommand -> GameState -> IO GameState
doGameStep proc state = do
  let (g, g')           = split $ state ^. randomGen
  let shuffledHivelings = shuffle' hivelings (length hivelings) g

  stdGenRef             <- newIORef g'
  hivelingsWithDecision <- mapM (takeDecision stdGenRef) shuffledHivelings
  g''                   <- readIORef stdGenRef

  return $ foldl' applyDecision (state & randomGen .~ g'') hivelingsWithDecision
 where
  hivelings :: [Hiveling']
  hivelings = state ^.. entities . each . asHiveling
  takeDecision :: IORef StdGen -> Hiveling' -> IO (Hiveling', Decision)
  takeDecision stdGenRef hiveling = do
    g <- readIORef stdGenRef
    let (r, g') = next g
    decision <- getHiveMindDecision
      proc
      Client.Input
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
    writeIORef stdGenRef g'
    return (hiveling, decision)
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

applyDecision :: GameState -> (Hiveling', Decision) -> GameState
applyDecision state (h, decision) =
  (case decision of
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
            state & hiveling . details . hasNutrition .~ True & entities %~ filter
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
    AppState { _gameState             = startingState
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
      continue $ s { _gameState       = startingState
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
  nextState <- liftIO $ doGameStep (s ^. hiveMindProcess) (s ^. gameState)
  continue $ s & iteration +~ 1 & gameState .~ nextState

-- Rendering
drawGameState :: Bool -> AppState -> Widget Name
drawGameState minimode state =
  let ((xLower, yLower), (xUpper, yUpper)) =
          if minimode then ((-20, -20), (20, 20)) else state ^. renderArea
  in  vBox
        [ hBox [ renderPosition (x, -y) | x <- [xLower .. xUpper] ]
        | y <- [yLower .. yUpper]
        ]
 where
  fillPosition :: Char -> [String]
  fillPosition c = if minimode then [[c]] else replicate 3 $ replicate 5 c
  renderPosition :: Position -> Widget Name
  renderPosition p = clickable p . str . unlines . obscureInvisible p $ maybe
    (fillPosition ' ')
    (^. details . to render)
    (pointsOfInterest !? p)
  pointsOfInterest :: Map Position Entity'
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
  hivelings :: [Hiveling']
  hivelings = state ^.. gameState . entities . each . asHiveling
  noHivelingSees :: Position -> Bool
  noHivelingSees p = not $ any (`sees` p) hivelings
  noHighlightClose :: Position -> Bool
  noHighlightClose p = all ((> 2.5) . distance p) highlights
  obscureInvisible :: Position -> [String] -> [String]
  obscureInvisible p s
    | state ^. hideUnseen && noHighlightClose p && noHivelingSees p
    = fillPosition '?'
    | otherwise
    = s
  render :: EntityDetails' -> [String]
  render d = case d of
    Nutrition    -> if minimode then ["*"] else [" .*. ", "* * *", " '*' "]
    HiveEntrance -> if minimode then ["O"] else ["/---\\", "| . |", "\\---/"]
    Pheromone    -> if minimode then ["~"] else [" .~. ", " ~.~ ", "  ~  "]
    Obstacle     -> fillPosition 'X'
    Hiveling h   -> if minimode then ["H"] else renderHiveling h
  renderHiveling :: HivelingDetails -> [String]
  renderHiveling h =
    let carried = [if h ^. hasNutrition then '*' else '.']
    in  case h ^. orientation of
          None ->
            [ "\\ " ++ carried ++ " /" -- \ * /
            , "/ | \\" --                 / | \
            , "/   \\" --                 /   \
            ]
          Clockwise ->
            [ "\\ \\ /" --          \ \ /
            , " ---" ++ carried --   ---*
            , "/ / \\" --           / / \
            ]
          Back ->
            [ "\\   /" --                 \   /
            , "\\ | /" --                 \ | /
            , "/ " ++ carried ++ " \\" -- / * \
            ]
          Counterclockwise ->
            [ "\\ \\ /" --         \ \ /
            , carried ++ "--- " -- *---
            , "/ / \\" --          / / \
            ]


scoreWidget :: AppState -> Widget Name
scoreWidget s =
  labeledBorder "Score" $ padLeftRight 5 . str . show $ s ^. gameState . score

selectedEntitiesWidget :: AppState -> Widget Name
selectedEntitiesWidget s = labeledBorder "Selected" $ if null highlights
  then padLeftRight 10 $ str "Nothing selected"
  else vBox (highlightBox <$> highlights)
 where
  highlights :: [Entity']
  highlights = sortOn
    (^. base . zIndex . to Down)
    (s ^.. gameState . entities . each . filtered (^. base . highlighted))
  highlightBox :: Entity' -> Widget Name
  highlightBox e =
    C.hCenter
      $ labeledBorder (e ^. base . position . to (\(x, y) -> (x, -y)) . to show)
      .  txt
      .  toStrict
      .  info
      $  e
      ^. details
  info :: EntityDetails' -> Text
  info (Hiveling d) = pShowNoColor d
  info d            = pShowNoColor d

helpWidget :: Widget Name
helpWidget = labeledBorder "Help" $ vBox (renderCommand <$> commands)
 where
  commands :: [(String, String)]
  commands =
    [ ("?"                      , "Goto this help page")
    , ("d"                      , "Goto selected entities view")
    , ("m"                      , "Goto minimap")
    , ("w"                      , "Goto main page (world)")
    , ("<Esc>/<Ctrl-c>/<Ctrl-d>", "Quit")
    , ("<Space>"                , "Pause / resume")
    , ("<Enter>"                , "Perform single step")
    , ("1-4"                    , "Set speed")
    , ("Arrow keys"             , "Move view")
    , ("v"                      , "Toggle visibility indication")
    , ("<Mouse-Left>"           , "Select entity for inspection")
    ]
  maxKeyLen :: Int
  maxKeyLen = maximum (length . fst <$> commands)
  renderCommand :: (String, String) -> Widget Name
  renderCommand (key, explanation)
    = str (key ++ replicate (maxKeyLen - length key + 2) ' ')
      <+> str explanation

worldWidget :: AppState -> Widget Name
worldWidget s = labeledBorder "World" (drawGameState False s)

minimapWidget :: AppState -> Widget Name
minimapWidget s = labeledBorder "Minimap" (drawGameState True s)

drawUI :: AppState -> [Widget Name]
drawUI s = [C.hCenter . labeledBorder "Hive Mind" $ page]
 where
  page = case s ^. currentPage of
    HelpPage         -> C.hCenter helpWidget
    Minimap          -> C.hCenter $ minimapWidget s
    SelectedEntities -> C.hCenter $ selectedEntitiesWidget s
    World            -> C.hCenter (scoreWidget s) <=> C.hCenter (worldWidget s)

labeledBorder :: String -> Widget a -> Widget a
labeledBorder label =
  withBorderStyle BS.unicode . B.borderWithLabel (padLeftRight 1 $ str label)
