module DemoMind
  ( runDemo
  , hivelingMind
  )
where

import           Common                         ( Position
                                                , base
                                                , details
                                                , EntityDetails(..)
                                                , Rotation(..)
                                                , Decision(..)
                                                , hPrintFlush
                                                )
import           Client                         ( Entity'
                                                , position
                                                , Input(..)
                                                , Hiveling'
                                                , closeEntities
                                                , hasNutrition
                                                , lastDecision
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
  workTowards _ (0, 0) = Move
  workTowards d (0, 1) = d
  workTowards _ (x, y) | y >= abs x  = Move
                       | -y >= abs x = Turn Back
                       | x < 0       = Turn Counterclockwise
                       | otherwise   = Turn Clockwise
  randomWalk :: Decision
  randomWalk = case hiveling ^. details . lastDecision of
    Turn _ -> Move
    _ ->
      Turn
        $ let minDirection = minBound :: Rotation
              maxDirection = maxBound :: Rotation
              (r, _)       = randomR
                (fromEnum minDirection, fromEnum maxDirection)
                (input ^. randomSeed . to mkStdGen)
          in  toEnum r

