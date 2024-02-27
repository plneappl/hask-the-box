module Main.World 
  ( World(..)
  ) where

import Main.ElementState

data World = World 
  { leds :: [Maybe ElementState]
  , switches :: [ElementState]
  }