module UI.Renderer
  ( renderWorld
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
  return anchorTopLeft

-- for debugging
renderWorld1 (screenX, screenY) _ =
  return $
    Pictures
      [ Translate 0 0 $ scaleUnif 0.1 $ Text "0, 0"
      , Translate 80 0 $ scaleUnif 0.1 $ Text "80, 0"
      , Translate 160 0 $ scaleUnif 0.1 $ Text "160, 0"
      , Translate (screenX / 2) 0 $ scaleUnif 0.1 $ Text "screenX, 0"
      , Translate (-80) 0 $ scaleUnif 0.1 $ Text "-80, 0"
      , Translate (-160) 0 $ scaleUnif 0.1 $ Text "-160, 0"
      , Translate (-screenX / 2) 0 $ scaleUnif 0.1 $ Text "-screenX, 0"
      , Translate 0 80 $ scaleUnif 0.1 $ Text "0, 80"
      , Translate 0 160 $ scaleUnif 0.1 $ Text "0, 160"
      , Translate 0 (screenY / 2) $ scaleUnif 0.1 $ Text "0, screenY"
      , Translate 0 (-80) $ scaleUnif 0.1 $ Text "0, -80"
      , Translate 0 (-160) $ scaleUnif 0.1 $ Text "0, -160"
      , Translate 0 (-screenY / 2) $ scaleUnif 0.1 $ Text "0, -screenY"
      ]