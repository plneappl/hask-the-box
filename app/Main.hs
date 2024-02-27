module Main (main) where

import Main.ElementState
import Main.Logic
import Main.World
import qualified Main.Color as C
import Graphics.Gloss(Display(InWindow), Color, white, Point)
import Graphics.Gloss.Interface.IO.Game (Event(..), playIO, Key(..), MouseButton(..), KeyState(..))
import UI.Renderer(renderWorld)
import UI.UiElement
import UI.Elements.Box
import UI.Elements.LED
import UI.Elements.Button
import UI.Elements.Switch

initialState :: World
initialState = World 
  { removedLeds = []
  , leds = fmap Just $ fmap (\c -> ElementState c False) [C.Red, C.Green, C.Blue, C.Yellow]
  , switches = fmap (\c -> ElementState c False) [C.Red, C.Green, C.Blue, C.Yellow]
  }

screenWidth :: Int
screenHeight :: Int
--screenWidth = 1366
--screenHeight = 768
screenWidth = 1500
screenHeight = 1000
screenSize :: Point
screenSize = (fromIntegral screenWidth, fromIntegral screenHeight)

fps :: Int
fps = 60

bgColor :: Color 
bgColor = white

displayMode :: Display
--displayMode = FullScreen (screenWidth, screenHeight)
displayMode = InWindow "The Box" (screenWidth, screenHeight) (0, 0)

eventHandler :: Event -> World -> IO World
eventHandler (EventKey (MouseButton LeftButton) Down _ pos) w = handleClick pos w >>= simpleLogic
eventHandler _ w = return w

handleClick :: Point -> World -> IO World
handleClick (posx, posy) w = do
  let screenX = posx + (fromIntegral screenWidth) / 2
  let screenY = - posy + (fromIntegral screenHeight) / 2
  let (uiWidth, uiHeight) = size ui
  if screenX < uiWidth && screenY < uiHeight then
    onClick ui (screenX, screenY) w
  else 
    return w

onTick :: Float -> World -> IO World
onTick _ it = return it

printHandler :: String -> ClickHandler
printHandler msg _ w = putStrLn msg >> return w

ui :: UiElement
ui = vbox (0, 0) 15 
  [ button "Shuffle1" (printHandler "btn1 clicked")
  , hbox (20, 0) 50 
    [ removedLed 0
    , removedLed 1
    , removedLed 2
    , removedLed 3
    ]
  , hbox (20, 0) 50 
    [ slottedLed 0
    , slottedLed 1
    , slottedLed 2
    , slottedLed 3
    ]
  , hbox (0, 0) 10 
    [ switch 0
    , switch 1
    , switch 2
    , switch 3
    ]
  , button "Shuffle2" (printHandler "btn2 clicked")
  ]

main :: IO ()
main = do
  playIO displayMode bgColor fps initialState (renderWorld screenSize ui) eventHandler onTick
