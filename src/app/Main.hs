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
                                                , Entity(..)
                                                , asHiveling
                                                , base
                                                , details
                                                , EntityBase(..)
                                                , identifier
                                                , position
                                                , zIndex
                                                , highlighted
                                                , EntityDetails(..)
                                                , HivelingProps(..)
                                                , lastDecision
                                                , hasNutrition
                                                , spreadsPheromones
                                                , Hiveling'
                                                , hivelingProps
                                                , HivelingMindInput(..)
                                                , Position
                                                , Direction(..)
                                                , go
                                                , distance
                                                , relativePosition
                                                , Decision(..)
                                                , DecisionType(..)
                                                , hPrintFlush
                                                )
import           System.Process                 ( ProcessHandle
                                                , runInteractiveCommand
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
import           Data.List                      ( maximumBy
                                                , sortOn
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
                                                , AttrMap
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

type Name = Position
type InteractiveProcess = (Handle, Handle, Handle, ProcessHandle)
data AppEvent = AdvanceGame deriving (Eq, Show, Ord)
data AppState = AppState {
  _gameState :: !GameState
 ,_hiveMindProcess :: !InteractiveProcess
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
  new = Entity
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
    $  (   ( Hiveling $ HivelingProps { _lastDecision = Decision Move Center
                                      , _hasNutrition = False
                                      , _spreadsPheromones = False
                                      }
           ,
           )
       <$> hivelings
       )
    ++ ((HiveEntrance, ) <$> entrances)
    ++ ((Obstacle, ) <$> topAndBottom)
    ++ ((Obstacle, ) <$> sides)
    ++ ((Nutrition, ) <$> nutrition)
 where
  hivelings    = [(1, 4), (-3, 12), (0, -6), (2, 2)]
  entrances    = (,) <$> [-5, 5] <*> [-5, 5]
  topAndBottom = (,) <$> [-9 .. 9] <*> [-20, -19, 19, 20]
  sides        = (,) <$> [-10, 10] <*> [-20 .. 20]
  nutrition    = (,) <$> [-5 .. 5] <*> [-15, -14, 0, 14, 15]

sees :: Hiveling' -> Position -> Bool
sees h p = distance p (h ^. _1 . position) < 6

doGameStep :: InteractiveProcess -> GameState -> IO GameState
doGameStep proc state = do
  -- TODO: This is a hack. Don't use IORef for State...
  stdGenRef             <- newIORef (state ^. randomGen)
  -- TODO: Shuffle
  hivelingsWithDecision <- mapM (takeDecision stdGenRef) hivelings
  gen                   <- readIORef stdGenRef
  return $ foldl applyDecision (state & randomGen .~ gen) hivelingsWithDecision
 where
  hivelings :: [Hiveling']
  hivelings = state ^.. entities . each . asHiveling . _Just
  takeDecision :: IORef StdGen -> Hiveling' -> IO (Hiveling', Decision)
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
                ((^. base . identifier) `isNot` (hiveling ^. _1 . identifier))
          .   filtered ((hiveling `sees`) . (^. base . position))
          &   each
          %~  forHivelingMind hiveling
        , _currentHiveling = hiveling ^. _2
        , _randomSeed      = r
        }
    writeIORef stdGenRef g'
    return (hiveling, decision)
  forHivelingMind :: Hiveling' -> Entity -> Entity
  forHivelingMind hiveling e =
    e
      &  base
      .  position
      %~ relativePosition (hiveling ^. _1 . position)
      &
      -- Hivelings don't know about ids
         base
      .  identifier
      .~ -1

applyDecision :: GameState -> (Hiveling', Decision) -> GameState
applyDecision state (hiveling, decision@(Decision t direction)) =
  storeDecision $ case t of
    Move   -> case topEntityAtTarget of
      Just (Entity _ Obstacle    ) -> state & score -~ 1
      Just (Entity _ (Hiveling _)) -> state
      Just (Entity b' _) ->
        state
          &  currentEntity
          .  base
          %~ (position .~ targetPos)
          .  (zIndex .~ (b' ^. zIndex) + 1)
      Nothing ->
        state & currentEntity . base %~ (position .~ targetPos) . (zIndex .~ 0)
    Pickup -> case topEntityAtTarget of
      Just (Entity _ Nutrition) -> if hiveling ^. _2 . hasNutrition
        then state
        else
          state
          &  currentHivelingProps
          .  hasNutrition
          .~ True
          &  entities
          %~ filter (Just `isNot` topEntityAtTarget)
      _                         -> state
    Drop   -> case topEntityAtTarget of
      Just (Entity _ HiveEntrance) -> if hiveling ^. _2 . hasNutrition
        then state & currentHivelingProps . hasNutrition .~ False & score +~ 10
        else state
      Just _                       -> state
      Nothing ->
        state
          &  score
          -~ 100 -- No food waste!
          &  currentHivelingProps
          .  hasNutrition
          .~ False
 where
  storeDecision :: GameState -> GameState
  storeDecision = currentHivelingProps . lastDecision .~ decision
  targetPos :: Position
  targetPos = (hiveling ^. _1 . position) `go` direction
  topEntityAtTarget :: Maybe Entity
  topEntityAtTarget =
    maximumByMay (comparing (^. base . zIndex))
      $   state
      ^.. entities
      .   each
      .   filtered ((^. base . position) `is` targetPos)
      . filtered ((^. base . identifier) `isNot` (hiveling ^. _1 . identifier))
  currentEntity :: Traversal' GameState Entity
  currentEntity =
    entities
      . each
      . filtered ((^. base . identifier) `is` (hiveling ^. _1 . identifier))
  currentHivelingProps :: Traversal' GameState HivelingProps
  currentHivelingProps = currentEntity . hivelingProps . _Just


getHiveMindDecision :: InteractiveProcess -> HivelingMindInput -> IO Decision
getHiveMindDecision (hIn, hOut, _, _) input = do
  hPrintFlush hIn input
  ready <- hWaitForInput hOut 100
  if ready
    then do
      output <- hGetLine hOut
      return $ read output
    else error "mind-command time-out"


-- App plumbing
data Flag = Help | MindCommand String | RunAsDemoMind deriving (Eq, Show)
flags :: [OptDescr Flag]
flags =
  [ Option ['d']
           ["demo-mind"]
           (NoArg RunAsDemoMind)
           "Run program as demo hiveling mind"
  , Option
    ['m']
    ["mind-command"]
    (ReqArg MindCommand "STR")
    "Command to continiously read HivlingMindInput from stdin and write Decision to stdout"
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
          else case findMindCommand args of
            Just h -> runApp h args
            _      -> failure ["Missing argument mind-command"]
    (_   , _, errors) -> failure errors
 where
  findMindCommand :: [Flag] -> Maybe String
  findMindCommand ((MindCommand h) : _ ) = Just h
  findMindCommand (_               : xs) = findMindCommand xs
  findMindCommand []                     = Nothing
  header :: String
  header = "Usage"
  failure :: [String] -> IO ()
  failure errors = do
    hPutStrLn stderr (unlines $ errors ++ [usageInfo header flags])
    exitWith (ExitFailure 1)


runApp :: String -> [Flag] -> IO ()
runApp mindCommand _ = do
  -- start hive mind command
  _hiveMindProcess@(hIn, hOut, _, _) <- runInteractiveCommand mindCommand
  _                                  <- hSetBinaryMode hIn False
  _                                  <- hSetBinaryMode hOut False

  -- channel to inject events into main loop
  _running                           <- newIORef (False, 100)
  chan                               <- newBChan 10
  _                                  <- advanceGame _running chan

  initialVty                         <- buildVty
  void $ customMain
    initialVty
    buildVty
    (Just chan)
    app
    AppState { _gameState  = startingState
             , _iteration  = 0
             , _hideUnseen = False
             , ..
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
          , appAttrMap      = const theMap
          }

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
    %~ (\e -> e & highlighted .~ e ^. position . to (== p))
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
      $   (\e -> (e ^. base . position, e))
      <$> state
      ^.  gameState
      .   entities
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
  hivelings = state ^.. gameState . entities . each . asHiveling . _Just
  noHivelingSees :: Position -> Bool
  noHivelingSees p = and $ not . (`sees` p) <$> hivelings
  noHighlightClose :: Position -> Bool
  noHighlightClose p = and $ (> 2.5) . distance p <$> highlights
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
    Hiveling h | h ^. hasNutrition && h ^. spreadsPheromones -> "U*"
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
  info (Hiveling d) = pShowNoColor d
  info d            = pShowNoColor d

theMap :: AttrMap
theMap = attrMap V.defAttr []

labeledBorder :: String -> Widget a -> Widget a
labeledBorder label =
  withBorderStyle BS.unicodeBold
    . B.borderWithLabel (padLeftRight 1 $ str label)
