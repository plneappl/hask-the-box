module Main (main) where

import Main.ElementState
import Main.World
import Util.FourOf as F
import qualified Main.Color as C
import Graphics.Gloss(Display(InWindow), Color, white, Point)
import Graphics.Gloss.Interface.IO.Game (Event, playIO)
import UI.Renderer(renderWorld)

initialState :: World
initialState = World {
  leds = fmap Just $ fmap (\c -> ElementState c False) $ FourOf C.Red C.Green C.Blue C.Yellow
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
eventHandler _ it = return it

onTick :: Float -> World -> IO World
onTick _ it = return it

main :: IO ()
main = do
  playIO displayMode bgColor fps initialState (renderWorld screenSize) eventHandler onTick
