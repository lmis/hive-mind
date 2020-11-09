module Render
  ( drawUI
  )
where

import           App                            ( AppState
                                                , renderArea
                                                , gameState
                                                , Name
                                                , gameLogs
                                                , Page(..)
                                                , currentPage
                                                , hideUnseen
                                                )
import           Game                           ( entities
                                                , position
                                                , Entity'
                                                , Hiveling'
                                                , EntityDetails'
                                                , zIndex
                                                , highlighted
                                                , asHiveling
                                                , sees
                                                , hasNutrition
                                                , orientation
                                                , score
                                                , HivelingDetails
                                                )
import           Common                         ( base
                                                , details
                                                , EntityDetails(..)
                                                , Position
                                                , Rotation(..)
                                                , distance
                                                )
import           GHC.Float                      ( int2Double )
import           Text.Pretty.Simple             ( pShowNoColor )
import           Data.Text.Lazy                 ( Text
                                                , unpack
                                                )
import           Data.Ord                       ( Down(..)
                                                , comparing
                                                )
import           Data.List                      ( maximumBy
                                                , sortOn
                                                )
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                , fromListWith
                                                )
import           Brick                          ( Widget
                                                , clickable
                                                , (<+>)
                                                , (<=>)
                                                , vBox
                                                , hBox
                                                , withBorderStyle
                                                , str
                                                , padLeftRight
                                                )
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import           Control.Lens                   ( (^.)
                                                , (^..)
                                                , each
                                                , to
                                                , filtered
                                                )

padCenterTo :: Int -> String -> String
padCenterTo n s = padRightTo n $ replicate half ' ' ++ s
  where half = round (int2Double (n - length s) / 2.0)
padRightTo :: Int -> String -> String
padRightTo n s = s ++ replicate (n - length s) ' '


labeledBorder :: String -> Widget a -> Widget a
labeledBorder label =
  withBorderStyle BS.unicode . B.borderWithLabel (padLeftRight 1 $ str label)

drawUI :: AppState -> [Widget Name]
drawUI s = [C.hCenter . labeledBorder "Hive Mind" $ page]
 where
  page = case s ^. currentPage of
    HelpPage         -> C.hCenter helpWidget
    Minimap          -> C.hCenter $ minimapWidget s
    SelectedEntities -> C.hCenter $ selectedEntitiesWidget s
    World            -> C.hCenter (scoreWidget s) <=> C.hCenter (worldWidget s)
    Logs             -> C.hCenter $ logsWidget s

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
            [ "\\ " ++ carried ++ " /" -- \ . /
            , "/ | \\" --                 / | \
            , "/   \\" --                 /   \
            ]
          Clockwise ->
            [ "\\ \\ /" --          \ \ /
            , " ---" ++ carried --   ---.
            , "/ / \\" --           / / \
            ]
          Back ->
            [ "\\   /" --                 \   /
            , "\\ | /" --                 \ | /
            , "/ " ++ carried ++ " \\" -- / . \
            ]
          Counterclockwise ->
            [ "\\ \\ /" --         \ \ /
            , carried ++ "--- " -- .---
            , "/ / \\" --          / / \
            ]


scoreWidget :: AppState -> Widget Name
scoreWidget s =
  labeledBorder "Score"
    $  padLeftRight 1
    .  str
    .  padCenterTo 18
    .  show
    $  s
    ^. gameState
    .  score

selectedEntitiesWidget :: AppState -> Widget Name
selectedEntitiesWidget s = labeledBorder "Selected" $ if null highlights
  then str $ padCenterTo 20 "Nothing selected"
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
      .  str
      .  padCenterTo 20
      .  unpack
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
    , ("l"                      , "Goto logs")
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
  renderCommand (key, explanation) =
    str (padRightTo (maxKeyLen + 2) key) <+> str explanation

logsWidget :: AppState -> Widget Name
logsWidget s =
  labeledBorder "Logs"
    $ vBox (str . padRightTo 50 <$> zipWith (++) prefixes logs)
 where
  logs     = s ^. gameLogs
  prefixes = (++ ": ") . show <$> [length logs, length logs - 1 .. 0]

worldWidget :: AppState -> Widget Name
worldWidget s = labeledBorder "World" (drawGameState False s)

minimapWidget :: AppState -> Widget Name
minimapWidget s = labeledBorder "Minimap" (drawGameState True s)
