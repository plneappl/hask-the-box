module UI.UiState
  ( UiState(..),
    ElementState(..)
  ) where

import Main.ElementState
import Util.FourOf(FourOf)



data UiState = UiState {
  leds :: FourOf ElementState,
  buttons :: FourOf ElementState
}
