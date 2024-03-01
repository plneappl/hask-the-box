module Main.Logic
  ( theLogic
  )
where

import Main.ElementState
import Main.World

theLogic :: Either Float () -> World -> IO World
theLogic = simpleLogic

-- turn on LEDs in a 1-1 correlation
simpleLogic :: Either Float () -> World -> IO World
simpleLogic _ w = do
  let areOn = map switchOn $ switches w
  let newLeds = zipWith setState (leds w) areOn
  return w{leds = newLeds}
 where
  setState Nothing _ = Nothing
  setState (Just (LedState col _)) newIsOn = Just $ LedState col newIsOn
