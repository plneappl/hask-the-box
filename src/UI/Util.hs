module UI.Util
  ( toGloss
  , scaleUnif
  , render
  , isHit
  , listSet
  , replaceFirstNothing
  , darken
  , lighten
  , (!?)
  , (?!?)
  ) where

import qualified Graphics.Gloss as G
import qualified Main.Color as C
import Main.World
import UI.UiElement

red :: G.Color
red = G.makeColor 0.8 0.2 0.2 1

green :: G.Color
green = G.makeColor 0.2 0.8 0.2 1

yellow :: G.Color
yellow = G.makeColor 0.88 0.75 0.15 1

blue :: G.Color
blue = G.makeColor 0.3 0.3 1 1

toGloss :: C.Color -> G.Color
toGloss C.Red = red
toGloss C.Green = green
toGloss C.Blue = blue
toGloss C.Yellow = yellow

darken :: G.Color -> G.Color
darken = G.dim . G.dim . G.dim

lighten :: G.Color -> G.Color
lighten = G.light

scaleUnif :: Float -> G.Picture -> G.Picture
scaleUnif s = G.Scale s s

render :: World -> UiElement -> IO G.Picture
render w it = drawSelf it w

isHit :: G.Point -> G.Point -> UiElement -> Bool
isHit (offsetX, offsetY) (posX, posY) child =
  let
    (sizeX, sizeY) = size child
    maxX = offsetX + sizeX
    maxY = offsetY + sizeY
   in
    offsetX <= posX && posX < maxX && offsetY <= posY && posY < maxY

listSet :: [a] -> Int -> a -> [a]
listSet xs i x = take i xs ++ [x] ++ drop (i + 1) xs

(!?) :: [a] -> Int -> Maybe a
as !? idx
  | length as > idx = Just $ as !! idx
  | otherwise = Nothing

(?!?) :: [Maybe a] -> Int -> Maybe a
as ?!? idx
  | length as > idx = as !! idx
  | otherwise = Nothing

replaceFirstNothing :: [Maybe a] -> a -> [Maybe a]
replaceFirstNothing (Nothing : xs) a = Just a : xs
replaceFirstNothing (x : xs) a = x : replaceFirstNothing xs a
replaceFirstNothing [] _ = []