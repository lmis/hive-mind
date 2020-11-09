{-# LANGUAGE RankNTypes #-}
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
                                                , closeEntities
                                                , hasNutrition
                                                , recentDecisions
                                                , currentHiveling
                                                , randomSeed
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           System.Random                  ( mkStdGen
                                                , randomR
                                                )
import           Control.Lens                   ( Traversal'
                                                , (^?)
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

entityAt :: Rotation -> Traversal' Input Entity'
entityAt rotation =
  closeEntities
    . each
    . filtered (\e -> (e ^. base . position) == rotation2Pos rotation)
 where
  rotation2Pos :: Rotation -> Position
  rotation2Pos None             = (0, 1)
  rotation2Pos Clockwise        = (1, 0)
  rotation2Pos Back             = (0, -1)
  rotation2Pos Counterclockwise = (-1, 0)

randomRotation :: Input -> Rotation
randomRotation input =
  let minDirection = minBound :: Rotation
      maxDirection = maxBound :: Rotation
      (r, _)       = randomR (fromEnum minDirection, fromEnum maxDirection)
                             (input ^. randomSeed . to mkStdGen)
  in  toEnum r

searchFood :: Input -> Decision
searchFood = search (\e -> (e ^. details) == Nutrition) Pickup

bringFoodHome :: Input -> Decision
bringFoodHome = search (\e -> (e ^. details) == HiveEntrance) Drop


search :: (Entity' -> Bool) -> Decision -> Input -> Decision
search condition decision input
  | input ^? entityAt None . to condition == Just True = decision
  | otherwise = case input ^? closeEntities . each . filtered condition of
    Just e  -> workTowards decision (e ^. base . position)
    Nothing -> randomWalk
 where
  workTowards :: Decision -> Position -> Decision
  workTowards _ (0, 0) = Move
  workTowards d (0, 1) = d
  workTowards _ (x, y) | y >= abs x  = Move
                       | -y >= abs x = Turn Back
                       | x < 0       = Turn Counterclockwise
                       | otherwise   = Turn Clockwise
  randomWalk :: Decision
  randomWalk = case input ^. currentHiveling . details . recentDecisions of
    ((Turn _) : _) -> Move
    _              -> Turn $ randomRotation input

hivelingMind :: Input -> Decision
hivelingMind input =
  let decision = if input ^. currentHiveling . details . hasNutrition
        then bringFoodHome input
        else searchFood input
   -- Avoid other Hivelings
  in  case decision of
        Move          -> case input ^? entityAt None . details of
          Just (Hiveling _) -> Turn $ randomRotation input
          Just Obstacle     -> Turn $ randomRotation input
          _                 -> decision
        Turn rotation -> case input ^? entityAt rotation . details of
          Just (Hiveling _) -> Move
          Just Obstacle     -> Move
          _                 -> decision
        _             -> decision



