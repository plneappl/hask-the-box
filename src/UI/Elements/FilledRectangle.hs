module UI.Elements.FilledRectangle
  ( filledRectangle
  ) where

import Graphics.Gloss (Color, Picture (Color, Translate), Point, rectangleSolid)
import Main.World
import UI.UiElement

filledRectangle :: Point -> (World -> Color) -> UiElement
filledRectangle s computeCol =
  UiElement
    { name = "filled rectangle"
    , size = s
    , drawSelf = renderBox s computeCol
    , onClick = ignoreClicks
    }

renderBox :: Point -> (World -> Color) -> World -> IO Picture
renderBox (width, height) computeCol w =
  let
    glossColor = computeCol w
   in
    return $ Translate (width / 2) (height / 2) $ Color glossColor $ rectangleSolid width height