module DemoMind
  ( runDemo
  , hivelingMind
  )
where

import           Common                         ( is
                                                , Entity(..)
                                                , base
                                                , details
                                                , position
                                                , EntityDetails(..)
                                                , hasNutrition
                                                , HivelingMindInput(..)
                                                , closeEntities
                                                , currentHiveling
                                                , randomSeed
                                                , Direction(..)
                                                , path
                                                , direction2Offset
                                                , Decision(..)
                                                , DecisionType(..)
                                                , hPrintFlush
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           System.Random                  ( mkStdGen
                                                , randomR
                                                )
import           Lens.Micro                     ( (^?)
                                                , (^.)
                                                , has
                                                , each
                                                , to
                                                , filtered
                                                )
import           System.IO                      ( stdout )



runDemo :: IO ()
runDemo = forever $ do
  input <- getLine
  void . hPrintFlush stdout . hivelingMind . read $ input

hivelingMind :: HivelingMindInput -> Decision
hivelingMind input
  | input ^. currentHiveling . hasNutrition = case
      findClose (== HiveEntrance)
    of
      Just obj -> path (obj ^. base . position) `followOrDo` Drop
      Nothing  -> randomWalk
  | otherwise = case findClose (== Nutrition) of
    Just obj -> path (obj ^. base . position) `followOrDo` Pickup
    Nothing  -> randomWalk
 where
  findClose :: (EntityDetails -> Bool) -> Maybe Entity
  findClose t = input ^? closeEntities . each . filtered (t . (^. details))
  followOrDo :: [Direction] -> DecisionType -> Decision
  followOrDo (Center : p) t = p `followOrDo` t
  followOrDo []           t = Decision t Center
  followOrDo [direction]  t = Decision t direction
  followOrDo (direction : _) _ =
    if has
         ( closeEntities
         . each
         . filtered ((^. base . position) `is` direction2Offset direction)
         )
         input
      then randomWalk
      else Decision Move direction
  randomWalk :: Decision
  randomWalk =
    Decision Move
      $ let minDirection = minBound :: Direction
            maxDirection = maxBound :: Direction
            (r, _) = randomR (fromEnum minDirection, fromEnum maxDirection)
                             (input ^. randomSeed . to mkStdGen)
        in  toEnum r

