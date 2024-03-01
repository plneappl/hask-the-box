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
import UI.Util (listSet, toGloss, (!?))

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
  elemState = leds world !! ledNum
  renderLed Nothing = Blank
  renderLed (Just (ElementState color isOn)) = ledColor circle
   where
    circle = Translate (ledSize / 2) (ledSize / 2) $ ThickCircle (ledSize / 4) (ledSize / 2)
    baseColor = toGloss color
    ledColor = if isOn then Color baseColor else Color $ dim $ dim baseColor

drawRemovedLed :: Int -> World -> IO Picture
drawRemovedLed ledNum world = return $ renderLed elemState
 where
  elemState = (removedLeds $ world) !? ledNum
  renderLed Nothing = Blank
  renderLed (Just color) = ledColor circle
   where
    circle = Translate (ledSize / 2) (ledSize / 2) $ ThickCircle (ledSize / 4) (ledSize / 2)
    baseColor = toGloss color
    ledColor = Color $ dim $ dim baseColor

removeLed' :: Int -> ClickHandler
removeLed' ledNum _ world = removeLed ledNum world

removeLed :: Int -> World -> IO World
removeLed ledNum world = do
  let elemState = leds world !! ledNum
  let newStates = listSet (leds world) ledNum Nothing
  let oldRemoved = removedLeds world
  let newRemoved = case elemState of
        Just x -> oldRemoved ++ [color x]
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
    Just ledColor -> do
      let newRemoved = take ledNum oldRemoved ++ drop (ledNum + 1) oldRemoved
      let newStates = replaceFirstNothing (leds world) (ElementState ledColor False)
      return
        world
          { removedLeds = newRemoved
          , leds = newStates
          }

replaceFirstNothing :: [Maybe a] -> a -> [Maybe a]
replaceFirstNothing (Nothing : xs) a = Just a : xs
replaceFirstNothing (x : xs) a = x : replaceFirstNothing xs a
replaceFirstNothing [] _ = []
