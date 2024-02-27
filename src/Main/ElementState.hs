module Main.ElementState 
  ( ElementState(..)
  ) where

import Main.Color

data ElementState = ElementState { color :: Color, isOn :: Bool }