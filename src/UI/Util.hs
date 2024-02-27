module UI.Util
  ( toGloss
  , scaleUnif
  , render
  , isHit
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
  return p

isHit :: G.Point -> G.Point -> UiElement -> Bool
isHit (offsetX, offsetY) (posX, posY) child = let
  (sizeX, sizeY) = size child
  maxX = offsetX + sizeX
  maxY = offsetY + sizeY in
  offsetX <= posX && posX < maxX && offsetY <= posY && posY < maxY