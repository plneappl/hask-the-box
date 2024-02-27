module UI.Elements.FilledRectangle 
  ( filledRectangle
  ) where


import qualified Main.Color as C
import Graphics.Gloss(Point, Picture(Color, Translate), rectangleSolid, Color)
import Main.World
import UI.UiElement
import UI.Util(toGloss)

filledRectangle :: Point -> (World -> Color) -> UiElement
filledRectangle s computeCol = UiElement {
  name = "filled rect " ++ (show s),
  size = s,
  drawSelf = renderBox s computeCol,
  onClick = ignoreClicks
}

renderBox :: Point -> (World -> Color) -> World -> IO Picture
renderBox (width, height) computeCol w = let
  glossColor = computeCol w in
  return $ Translate (width / 2) (height / 2) $ Color glossColor $ rectangleSolid width height