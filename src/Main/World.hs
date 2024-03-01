module Main.World
  ( World (..)
  , Animation (..)
  , createAnimation
  , runAnimation
  , concatAnimations
  ) where

import Main.Color
import Main.ElementState

data World = World
  { removedLeds :: [Color]
  , leds :: [Maybe LedState]
  , switches :: [SwitchState]
  , removedSwitches :: [Color]
  , animation :: Maybe Animation
  , timeElapsed :: Float
  , randomSlots :: [Int]
  }

data Animation = Animation
  { nextAction :: World -> IO World
  , furtherActions :: [World -> IO World]
  }

concatAnimations :: Maybe Animation -> Maybe Animation -> Maybe Animation
concatAnimations Nothing x = x
concatAnimations x Nothing = x
concatAnimations (Just a1) (Just a2) =
  Just $
    Animation
      { nextAction = nextAction a1
      , furtherActions = furtherActions a1 ++ [nextAction a2] ++ furtherActions a2
      }

createAnimation :: [World -> IO World] -> Maybe Animation
createAnimation [] = Nothing
createAnimation (a : as) =
  Just $
    Animation
      { nextAction = a
      , furtherActions = as
      }

-- in Hz
animationSpeed :: Float
animationSpeed = 2
animationPeriod :: Float
animationPeriod = 1 / animationSpeed

runAnimation :: Float -> World -> IO World
runAnimation time w =
  case animation w of
    Nothing -> return w{timeElapsed = 0}
    Just a ->
      let time' = timeElapsed w + time
       in if time' < animationPeriod
            then return $ w{timeElapsed = time'}
            else do
              w1 <- nextAction a w
              let nextAnimation = case furtherActions a of
                    [] -> Nothing
                    (a1 : as) ->
                      Just $
                        Animation
                          { nextAction = a1
                          , furtherActions = as
                          }
              let timeElapsed' = case nextAnimation of
                    Just _ -> timeElapsed w1 - animationPeriod
                    Nothing -> 0
              return $ w1{animation = nextAnimation, timeElapsed = timeElapsed'}