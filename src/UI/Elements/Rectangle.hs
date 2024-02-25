module UI.Elements.Rectangle 
  ( rectangle
  ) where

import Graphics.Gloss(Point, Picture(Line))
import Main.World
import UI.UiElement

rectangle :: Point -> UiElement
rectangle s = UiElement {
  size = s,
  drawSelf = renderBox s,
  onClick = ignoreClicks
}

renderBox :: Point -> World -> IO Picture
renderBox (width, height) _ = return $ Line 
  [ (0.0, 0.0)
  , (width, 0.0)
  , (width, height)
  , (0.0, height)
  , (0.0, 0.0)
  ]
