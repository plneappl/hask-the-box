module Main.Logic
  ( simpleLogic
  )
where

import Main.ElementState
import Main.World

-- turn on LEDs in a 1-1 correlation
simpleLogic :: World -> IO World
simpleLogic w = do
  let areOn = map switchOn $ switches w
  let newLeds = zipWith setState (leds w) areOn
  return w{leds = newLeds}
 where
  setState Nothing _ = Nothing
  setState (Just (LedState col _)) newIsOn = Just $ LedState col newIsOn
