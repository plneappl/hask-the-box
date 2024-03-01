module UI.Elements.LED
  ( slottedLed
  , removedLed
  , removeLed
  , setLed
  ) where

import Graphics.Gloss (Picture (..), dim)
import Main.ElementState
import Main.World
import UI.UiElement
import UI.Util (darken, listSet, replaceFirstNothing, toGloss, (!?))

slottedLed :: Int -> UiElement
slottedLed ledNum =
  UiElement
    { name = name
    , size = (ledSize, ledSize)
    , drawSelf = drawLed ledNum
    , onClick = removeLed' ledNum
    }
 where
  name = "LED " ++ show ledNum

removedLed :: Int -> UiElement
removedLed ledNum =
  UiElement
    { name = name
    , size = (ledSize, ledSize)
    , drawSelf = drawRemovedLed ledNum
    , onClick = setLed' ledNum
    }
 where
  name = "LED spare " ++ show ledNum

ledSize :: Float
ledSize = 40

drawLed :: Int -> World -> IO Picture
drawLed ledNum world = return $ renderLed elemState
 where
  offset = Translate (ledSize / 2) (ledSize / 2)
  elemState = leds world !! ledNum
  renderLed Nothing = offset $ Circle (ledSize / 2)
  renderLed (Just (LedState color isOn)) = ledColor circle
   where
    circle = offset $ ThickCircle (ledSize / 4) (ledSize / 2)
    baseColor = toGloss color
    ledColor = if isOn then Color baseColor else Color $ dim $ dim baseColor

drawRemovedLed :: Int -> World -> IO Picture
drawRemovedLed ledNum world = return $ renderLed elemState
 where
  offset = Translate (ledSize / 2) (ledSize / 2)
  elemState = (removedLeds $ world) !? ledNum
  renderLed Nothing = offset $ Circle (ledSize / 2)
  renderLed (Just color) = ledColor circle
   where
    circle = offset $ ThickCircle (ledSize / 4) (ledSize / 2)
    baseColor = toGloss color
    ledColor = Color $ darken baseColor

removeLed' :: Int -> ClickHandler
removeLed' ledNum _ world = removeLed ledNum world

removeLed :: Int -> World -> IO World
removeLed ledNum world = do
  let elemState = leds world !! ledNum
  let newStates = listSet (leds world) ledNum Nothing
  let oldRemoved = removedLeds world
  let newRemoved = case elemState of
        Just x -> oldRemoved ++ [ledColor x]
        Nothing -> oldRemoved
  return
    world
      { removedLeds = newRemoved
      , leds = newStates
      }

setLed' :: Int -> ClickHandler
setLed' ledNum _ world = setLed ledNum world

setLed :: Int -> World -> IO World
setLed ledNum world = do
  let oldRemoved = removedLeds world
  let ledToPut = oldRemoved !? ledNum
  case ledToPut of
    Nothing -> return world
    Just col -> do
      let newRemoved = take ledNum oldRemoved ++ drop (ledNum + 1) oldRemoved
      let newStates = replaceFirstNothing (leds world) (LedState col False)
      return
        world
          { removedLeds = newRemoved
          , leds = newStates
          }
