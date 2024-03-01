module UI.Elements.Rectangle
  ( rectangle
  ) where

import Graphics.Gloss (Picture (Line), Point)
import Main.World
import UI.UiElement

rectangle :: Point -> UiElement
rectangle s =
  UiElement
    { name = "rect " ++ show s
    , size = s
    , drawSelf = renderBox s
    , onClick = ignoreClicks
    }

renderBox :: Point -> World -> IO Picture
renderBox (width, height) _ =
  return $
    Line
      [ (0.0, 0.0)
      , (width, 0.0)
      , (width, height)
      , (0.0, height)
      , (0.0, 0.0)
      ]
