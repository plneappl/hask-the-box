module Main.ElementState
  ( LedState (..)
    , SwitchState(..)
  )
where

import Main.Color

data LedState = LedState {ledColor :: Color, ledOn :: Bool}
data SwitchState = SwitchState {switchColor :: Maybe Color, switchOn :: Bool}