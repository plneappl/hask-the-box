{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Main (main) where

import Control.Monad (unless)
import Graphics.Gloss (Color, Display (InWindow), Point, animate, white)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), MouseButton (..), playIO)
import qualified Main.Color as C
import Main.ElementState
import Main.Logic
import Main.World
import System.Random
import UI.Elements.Box
import UI.Elements.Button
import UI.Elements.LED
import UI.Elements.Switch
import UI.Renderer (renderWorld, translateClick)
import UI.UiElement
import UI.Util (listSet)

initialRandomSlots :: StdGen -> [Int]
initialRandomSlots r = randomRs (0, 4) r

initialWorld :: StdGen -> World
initialWorld r =
  World
    { removedLeds = []
    , leds = Just <$> fmap (\c -> LedState c False) [C.Red, C.Green, C.Blue, C.Yellow]
    , switches = fmap (\c -> SwitchState c False) $ map Just $ [C.Red, C.Green, C.Blue, C.Yellow]
    , removedSwitches = []
    , animation = Nothing
    , timeElapsed = 0
    , randomSlots = initialRandomSlots r
    , logicState = resetLogicState
    }

screenWidth :: Int
screenHeight :: Int
screenWidth = 1366
screenHeight = 768

screenSize :: Point
screenSize = (fromIntegral screenWidth, fromIntegral screenHeight)

fps :: Int
fps = 60

bgColor :: Color
bgColor = white

displayMode :: Display
-- displayMode = FullScreen (screenWidth, screenHeight)
displayMode = InWindow "The Box" (screenWidth, screenHeight) (0, 0)

eventHandler :: Event -> World -> IO World
eventHandler (EventKey (MouseButton LeftButton) Down _ pos) w =
  case animation w of
    Nothing -> handleClick pos w >>= theLogic (Right ())
    Just _ -> return w
eventHandler _ w = return w

handleClick :: Point -> World -> IO World
handleClick pos w = do
  let uiSize@(uiWidth, uiHeight) = size ui
  let (screenX, screenY) = translateClick screenSize uiSize pos
  if screenX < uiWidth && screenY < uiHeight
    then onClick ui (screenX, screenY) w
    else return w

onTick :: Float -> World -> IO World
onTick time w = runAnimation time w >>= theLogic (Left time)

printHandler :: String -> ClickHandler
printHandler msg _ w = putStrLn msg >> return w

removeAllLeds :: Maybe Animation
removeAllLeds =
  createAnimation
    [ removeLed 0
    , removeLed 1
    , removeLed 2
    , removeLed 3
    ]

setAllLeds :: Maybe Animation
setAllLeds =
  createAnimation
    [ setRandomLed
    , setRandomLed
    , setRandomLed
    , setRandomLed
    ]

removeAllSwitches :: Maybe Animation
removeAllSwitches =
  createAnimation
    [ removeSwitch 0
    , removeSwitch 1
    , removeSwitch 2
    , removeSwitch 3
    ]

setAllSwitches :: Maybe Animation
setAllSwitches =
  createAnimation
    [ setRandomSwitch
    , setRandomSwitch
    , setRandomSwitch
    , setRandomSwitch
    ]

shuffleLeds :: Maybe Animation
shuffleLeds = concatAnimations removeAllLeds setAllLeds

shuffleSwitches :: Maybe Animation
shuffleSwitches = concatAnimations removeAllSwitches setAllSwitches

setRandomLed :: World -> IO World
setRandomLed w = do
  let (s, w1) = getRandomSlot w
  let remainingLeds = length $ removedLeds w1
  setLed (s `mod` remainingLeds) w1

setRandomSwitch :: World -> IO World
setRandomSwitch w = do
  let (s, w1) = getRandomSlot w
  let remainingSwitches = length $ removedSwitches w1
  setSwitchColor (s `mod` remainingSwitches) w1

turnOffRandomSwitch :: World -> IO World
turnOffRandomSwitch w = do
  let (s, w1) = getRandomSlot w
  let onSwitchIdxs = filter (switchOn . snd) $ zip [0 ..] $ switches w
  if (length onSwitchIdxs == 0)
    then return w
    else do
      let switchIdx = fst $ onSwitchIdxs !! (s `mod` (length onSwitchIdxs))
      return
        w1
          { switches = listSet (switches w1) switchIdx ((switches w1 !! switchIdx){switchOn = False})
          }

turnOffSwitches :: Maybe Animation
turnOffSwitches =
  createAnimation
    [ turnOffRandomSwitch
    , turnOffRandomSwitch
    , turnOffRandomSwitch
    , turnOffRandomSwitch
    ]

setAnimation :: Maybe Animation -> ClickHandler
setAnimation a _ w = return $ w{animation = a}

getRandomSlot :: World -> (Int, World)
getRandomSlot w = (head $ randomSlots w, w{randomSlots = tail $ randomSlots w})

ui :: UiElement
ui =
  vbox
    (0, 0)
    15
    [ button "Shuffle LEDs" (setAnimation shuffleLeds)
    , hbox
        (20, 0)
        50
        [ removedLed 0
        , removedLed 1
        , removedLed 2
        , removedLed 3
        ]
    , hbox
        (20, 0)
        50
        [ slottedLed 0
        , slottedLed 1
        , slottedLed 2
        , slottedLed 3
        ]
    , hbox
        (0, 0)
        10
        [ switch 0
        , switch 1
        , switch 2
        , switch 3
        ]
    , hbox
        (20, 0)
        50
        [ removedSwitch 0
        , removedSwitch 1
        , removedSwitch 2
        , removedSwitch 3
        ]
    , hbox
        (0, 0)
        48
        [ button "Shuffle Switches" (setAnimation shuffleSwitches)
        , button "Turn off" (setAnimation turnOffSwitches)
        ]
    ]

main :: IO ()
main = do
  r <- getStdGen
  playIO displayMode bgColor fps (initialWorld r) (renderWorld screenSize ui) eventHandler onTick
