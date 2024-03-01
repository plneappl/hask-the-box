{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Main (main) where

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
import UI.Renderer (renderWorld)
import UI.UiElement

initialRandomSlots :: StdGen -> [Int]
initialRandomSlots r = randomRs (0, 4) r

initialWorld :: StdGen -> World
initialWorld r =
  World
    { removedLeds = []
    , leds = Just <$> fmap (\c -> ElementState c False) [C.Red, C.Green, C.Blue, C.Yellow]
    , switches = fmap (\c -> ElementState c False) [C.Red, C.Green, C.Blue, C.Yellow]
    , animation = Nothing
    , timeElapsed = 0
    , randomSlots = initialRandomSlots r
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
    Nothing -> handleClick pos w >>= simpleLogic
    Just _ -> return w
eventHandler _ w = return w

handleClick :: Point -> World -> IO World
handleClick (posx, posy) w = do
  let screenX = posx + fromIntegral screenWidth / 2
  let screenY = -posy + fromIntegral screenHeight / 2
  let (uiWidth, uiHeight) = size ui
  if screenX < uiWidth && screenY < uiHeight
    then onClick ui (screenX, screenY) w
    else return w

onTick :: Float -> World -> IO World
onTick time w = runAnimation time w

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

shuffleLeds :: Maybe Animation
shuffleLeds = concatAnimations removeAllLeds setAllLeds

setRandomLed :: World -> IO World
setRandomLed w = do
  let (s, w1) = getRandomSlot w
  setLed s w1

setAnimation :: Maybe Animation -> ClickHandler
setAnimation a _ w = return $ w{animation = a}

getRandomSlot :: World -> (Int, World)
getRandomSlot w = (head $ randomSlots w, w{randomSlots = tail $ randomSlots w})

ui :: UiElement
ui =
  vbox
    (0, 0)
    15
    [ button "Shuffle1" (setAnimation shuffleLeds)
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
    , button "Shuffle2" (printHandler "btn2 clicked")
    ]

main :: IO ()
main = do
  r <- getStdGen
  playIO displayMode bgColor fps (initialWorld r) (renderWorld screenSize ui) eventHandler onTick
