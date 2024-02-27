module UI.Elements.Switch 
  ( switch
  ) where

import Graphics.Gloss(Point, dim, light)
import Main.World
import UI.UiElement
import UI.Elements.Box
import UI.Elements.Label
import UI.Elements.FilledRectangle
import UI.Elements.Rectangle
import UI.Elements.StackPane
import UI.Util(toGloss, listSet)
import Util.FourOf(iter, fromList)
import Main.ElementState

switch :: Int -> UiElement
switch switchNum = x { onClick = setSwitch switchNum } where
  x = hbox (0, 0) 0
    [ stackPane 
      [ label 12 "On"
      , rectangle switchHalf
      , filledRectangle switchHalf computeCol 
      ]
    , stackPane 
      [ label 12 "Off"
      , rectangle switchHalf
      , filledRectangle switchHalf computeCol 
      ]
    ]
  computeCol w = let
    states = iter $ switches w
    state = states !! switchNum 
    glossCol = light $ light $ toGloss $ color state in
    if isOn state then glossCol else dim $ dim glossCol

switchHalfWidth :: Float
switchHalfWidth = 40
switchSize :: Point
switchSize = (2*switchHalfWidth, switchHalfWidth)
switchHalf :: Point
switchHalf = (switchHalfWidth, switchHalfWidth)

setSwitch :: Int -> ClickHandler
setSwitch switchNum (xpos, _) w = do
  let states = iter $ switches w
  let state = states !! switchNum
  let newIsOn = xpos < switchHalfWidth
  let newState = state { isOn = newIsOn }
  let newStates = fromList $ listSet states switchNum newState
  let newWorld = w { switches = newStates }
  return newWorld 
