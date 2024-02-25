module UI.Elements.LED
  ( slottedLed
  ) where

import Graphics.Gloss(Color, Picture(..), Point, dim)
import UI.Util(toGloss)
import UI.UiElement
import Main.ElementState
import Main.World
import Util.FourOf

slottedLed :: Int -> UiElement
slottedLed ledNum = UiElement {
  size = (ledSize, ledSize),
  drawSelf = drawLed ledNum,
  onClick = removeLed
}

ledSize :: Float
ledSize = 40.0

drawLed :: Int -> World -> IO Picture
drawLed ledNum world = return $ renderLed elemState where
  elemState = (iter (leds $ world)) !! ledNum
  renderLed Nothing = Blank
  renderLed (Just (ElementState color isOn)) = ledColor circle
    where
      circle = Translate (ledSize / 2) (ledSize / 2) $ ThickCircle (ledSize / 4) (ledSize / 2)
      baseColor = toGloss color
      ledColor = if isOn then Color baseColor else Color $ dim $ dim baseColor

removeLed :: ClickHandler
removeLed _ = return

