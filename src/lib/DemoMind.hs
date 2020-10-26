module DemoMind
  ( runDemo
  , hivelingMind
  )
where

import           Common                         ( Position
                                                , base
                                                , details
                                                , EntityDetails(..)
                                                , hasNutrition
                                                , orientation
                                                , lastDecision
                                                , Direction(..)
                                                , closestDirection
                                                , offset2Direction
                                                , Decision(..)
                                                , hPrintFlush
                                                )
import           Client                         ( Entity'
                                                , position
                                                , Input(..)
                                                , Hiveling'
                                                , closeEntities
                                                , currentHiveling
                                                , randomSeed
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           System.Random                  ( mkStdGen
                                                , randomR
                                                )
import           Control.Lens                   ( (^?)
                                                , (^.)
                                                , each
                                                , to
                                                , filtered
                                                )
import           System.IO                      ( stdout )
import           Data.Maybe                     ( fromJust )



runDemo :: IO ()
runDemo = do
  hPrintFlush stdout "Hello World"
  forever $ do
    input <- getLine
    void . hPrintFlush stdout . hivelingMind . read $ input

hivelingMind :: Input -> Decision
hivelingMind input
  | hiveling ^. details . hasNutrition = case findClose (== HiveEntrance) of
    Just obj -> workTowards Drop (obj ^. base . position)
    Nothing  -> randomWalk
  | otherwise = case findClose (== Nutrition) of
    Just obj -> workTowards Pickup (obj ^. base . position)
    Nothing  -> randomWalk
 where
  hiveling :: Hiveling'
  hiveling = input ^. currentHiveling
  findClose :: (EntityDetails -> Bool) -> Maybe Entity'
  findClose t = input ^? closeEntities . each . filtered (t . (^. details))
  workTowards :: Decision -> Position -> Decision
  workTowards d (0, 0) = d
  workTowards d p
    | offset2Direction p == Just (hiveling ^. details . orientation) = d
    | closestDirection p == Just (hiveling ^. details . orientation) = Move
    | otherwise = Turn . fromJust $ closestDirection p
  randomWalk :: Decision
  randomWalk = case hiveling ^. details . lastDecision of
    Turn _ -> Move
    _ ->
      Turn
        $ let minDirection = minBound :: Direction
              maxDirection = maxBound :: Direction
              (r, _)       = randomR
                (fromEnum minDirection, fromEnum maxDirection)
                (input ^. randomSeed . to mkStdGen)
          in  toEnum r

