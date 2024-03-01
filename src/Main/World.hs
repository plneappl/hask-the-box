module Main.World
  ( World (..)
  ) where

import Main.Color
import Main.ElementState

data World = World
  { removedLeds :: [Color]
  , leds :: [Maybe ElementState]
  , switches :: [ElementState]
  }