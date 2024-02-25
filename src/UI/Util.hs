module UI.Util
  ( toGloss
  , scaleUnif
  , render
  ) where

import qualified Main.Color as C
import qualified Graphics.Gloss as G
import Main.World
import UI.UiElement

toGloss :: C.Color -> G.Color
toGloss C.Red    = G.red
toGloss C.Green  = G.green
toGloss C.Blue   = G.blue
toGloss C.Yellow = G.yellow

scaleUnif :: Float -> G.Picture -> G.Picture
scaleUnif s p = G.Scale s s p

render :: World -> UiElement -> IO G.Picture
render w it = do
  let (sizeX, sizeY) = size it
  p <- (drawSelf it) w
  return p -- $ G.Translate (sizeX / 2) (- sizeY / 2) p