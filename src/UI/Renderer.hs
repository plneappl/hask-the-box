module UI.Renderer
  ( renderWorld
  , translateClick
  ) where

import Graphics.Gloss
import Main.World
import UI.UiElement
import UI.Util (render, scaleUnif)

renderWorld :: Point -> UiElement -> World -> IO Picture
renderWorld (screenX, screenY) ui w = do
  let (sizeX, sizeY) = size ui
  p <- render w ui
  let anchorTopLeft = Translate (-screenX / 2) (screenY / 2 - sizeY) p
  let centered = Translate (-sizeX / 2) (-sizeY / 2) p
  return centered

translateClick :: Point -> Point -> Point -> Point
translateClick = translateCentered

translateTopLeft :: Point -> Point -> Point -> Point
translateTopLeft (screenX, screenY) _ (x, y) = (x + screenX / 2, -y + screenY / 2)

translateCentered :: Point -> Point -> Point -> Point
translateCentered _ (uiX, uiY) (x, y) = (x + uiX/2, -y + uiY/2)