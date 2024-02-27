module Main.World 
  ( World(..)
  ) where

import Util.FourOf
import Main.ElementState

data World = World 
  { leds :: FourOf (Maybe ElementState)
  , switches :: FourOf ElementState
  }