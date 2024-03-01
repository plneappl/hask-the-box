module Main.World
  ( World (..)
  , Animation
  , runAnimation
  ) where

import Main.Color
import Main.ElementState

data World = World
  { removedLeds :: [Color]
  , leds :: [Maybe ElementState]
  , switches :: [ElementState]
  , animation :: Maybe Animation
  }

data Animation = Animation
  { nextTick :: Int
  , nextAction :: World -> IO (Int, World)
  , furtherActions :: [World -> IO (Int, World)]
  }

runAnimation :: Int -> World -> IO World
runAnimation tick w =
  case animation w of
    Nothing -> return w
    Just a
      | tick < nextTick a -> return w
      | otherwise -> do
          (nt, w1) <- nextAction a w
          let nextAnimation = case furtherActions a of
                [] -> Nothing
                (a1 : as) ->
                  Just $
                    Animation
                      { nextTick = nt
                      , nextAction = a1
                      , furtherActions = as
                      }
          return $ w1{animation = nextAnimation}