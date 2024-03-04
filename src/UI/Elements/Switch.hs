module UI.Elements.Switch
  ( switch
  , removedSwitch
  , removeSwitch
  , setSwitchOn
  , setSwitchColor
  ) where

import Data.Maybe (isNothing)
import Graphics.Gloss (Point, dim, light, white)
import Main.ElementState
import Main.World
import UI.Elements.Box
import UI.Elements.FilledRectangle
import UI.Elements.Label
import UI.Elements.Rectangle
import UI.Elements.StackPane
import UI.UiElement
import UI.Util (darken, lighten, listSet, toGloss, (!?))

switch :: Int -> UiElement
switch switchNum =
  vbox
    (0, 0)
    0
    [ hbox
        (0, 0)
        0
        [ btnOn{onClick = setSwitchOn' True switchNum}
        , btnOff{onClick = setSwitchOn' False switchNum}
        ]
    , btnRemove{onClick = removeSwitch' switchNum}
    ]
 where
  btnOn =
    stackPane
      [ label 12 "On"
      , rectangle switchHalf
      , filledRectangle switchHalf computeCol
      ]
  btnOff =
    stackPane
      [ label 12 "Off"
      , rectangle switchHalf
      , filledRectangle switchHalf computeCol
      ]
  btnRemove =
    stackPane
      [ label 12 "X"
      , rectangle removeSwitchSize
      , filledRectangle removeSwitchSize (const $ dim white)
      ]
  computeCol w =
    let
      states = switches w
      state = states !! switchNum
      swCol = switchColor state
      glossCol = case swCol of
        Just c -> light $ light $ toGloss $ c
        Nothing -> dim white
     in
      if switchOn state then lighten glossCol else darken glossCol

removedSwitch :: Int -> UiElement
removedSwitch switchNum =
  uiElem{onClick = setSwitchColor' switchNum}
 where
  uiElem =
    stackPane
      [ rectangle switchHalf
      , filledRectangle switchHalf computeCol
      ]
  computeCol w =
    let
      states = removedSwitches w
     in
      if length states > switchNum then darken $ light $ light $ toGloss $ states !! switchNum else white

switchHalfWidth :: Float
switchHalfWidth = 40
switchHeight :: Float
switchHeight = switchHalfWidth
switchHalf :: Point
switchHalf = (switchHalfWidth, switchHeight)
removeSwitchWidth :: Float
removeSwitchWidth = 2 * switchHalfWidth
removeSwitchHeight :: Float
removeSwitchHeight = 20
removeSwitchSize :: Point
removeSwitchSize = (removeSwitchWidth, removeSwitchHeight)

setSwitchOn' :: Bool -> Int -> ClickHandler
setSwitchOn' newIsOn switchNum _ w = setSwitchOn newIsOn switchNum w

setSwitchOn :: Bool -> Int -> World -> IO World
setSwitchOn newIsOn switchNum w = do
  let states = switches w
  let state = states !! switchNum
  let newState = state{switchOn = newIsOn}
  let newStates = listSet states switchNum newState
  let newWorld = w{switches = newStates}
  return newWorld

removeSwitch' :: Int -> ClickHandler
removeSwitch' switchNum _ world = removeSwitch switchNum world

removeSwitch :: Int -> World -> IO World
removeSwitch switchNum world = do
  let elemState = switches world !! switchNum
  let newStates = listSet (switches world) switchNum $ SwitchState{switchColor = Nothing, switchOn = switchOn elemState}
  let oldRemoved = removedSwitches world
  let newRemoved = case switchColor elemState of
        Just x -> oldRemoved ++ [x]
        Nothing -> oldRemoved
  return
    world
      { switches = newStates
      , removedSwitches = newRemoved
      }

setSwitchColor' :: Int -> ClickHandler
setSwitchColor' switchNum _ w = setSwitchColor switchNum w

setSwitchColor :: Int -> World -> IO World
setSwitchColor switchNum world = do
  let oldRemoved = removedSwitches world
  let switchToPut = oldRemoved !? switchNum
  case switchToPut of
    Nothing -> return world
    Just col -> do
      let oldStates = switches world
      let posToPut = fst $ head $ filter (\(_, x) -> isNothing (switchColor x)) $ zip [0 ..] oldStates
      let oldState = oldStates !! posToPut
      let newRemoved = take switchNum oldRemoved ++ drop (switchNum + 1) oldRemoved
      let newStates = listSet oldStates posToPut (oldState{switchColor = Just col})
      return
        world
          { removedSwitches = newRemoved
          , switches = newStates
          }
