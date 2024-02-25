module UI.Elements.Label 
  ( label
  ) where

import Graphics.Gloss(Picture(..))
import Main.World
import UI.UiElement
import UI.Util(scaleUnif)

label :: Float -> String -> UiElement
label height text = UiElement {
  name = "label: " ++ text,
  size = (width, height),
  drawSelf = drawText scale text,
  onClick = \_ -> return
} where
  scale = height / baseHeight
  width :: Float
  width = scale * baseWidth * (fromIntegral $ length text)

baseWidth :: Float
baseWidth = 60

baseHeight :: Float
baseHeight = 110

drawText :: Float -> String -> World -> IO Picture
drawText scale text _ = return $ scaleUnif scale $ Text text