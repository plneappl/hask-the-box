module UI.Elements.Box
  ( hbox
  , vbox
  ) where

import Data.List(intersperse)
import Graphics.Gloss
import Main.World
import UI.UiElement
import UI.Util(render)
import Debug.Trace

hbox :: Point -> Float -> [UiElement] -> UiElement
hbox margin hseparator children = UiElement {
  size = bounds,
  drawSelf = layoutHorizontal bounds margin hseparator children,
  onClick = clickOnChildren children
} where 
  sizeX = sum $ intersperse hseparator $ map fst $ map size children
  sizeY = maximum $ map snd $ map size children
  bounds = (sizeX, sizeY)

vbox :: Point -> Float -> [UiElement] -> UiElement
vbox margin vseparator children = UiElement {
  size = bounds,
  drawSelf = layoutVertical bounds margin vseparator children,
  onClick = clickOnChildren children
} where 
  sizeX = maximum $ map fst $ map size children
  sizeY = sum $ intersperse vseparator $ map snd $ map size children
  bounds = (sizeX, sizeY)

layoutHorizontal :: Point -> Point -> Float -> [UiElement] -> World -> IO Picture
layoutHorizontal _ _ _ [] _ = return Blank
layoutHorizontal (sizeX, sizeY) margin hseparator children w = do
  pics <- mapM (render w) children
  let offsets = scanl (\a b -> a + b + hseparator) 0 $ map fst $ map size children
  return $ Pictures $ map (\(off, p) -> Translate off 0 p) $ zip (tail offsets) pics

layoutVertical :: Point -> Point -> Float -> [UiElement] -> World -> IO Picture
layoutVertical _ _ _ [] _ = return Blank
layoutVertical (sizeX, sizeY) margin vseparator children w = do
  pics <- mapM (render w) children
  let offsets = scanr (\a b -> a + b + vseparator) 0 $ map snd $ map size children
  return $ Pictures $ map (\(off, p) -> Translate 0 (off) p) $ zip (tail offsets) pics

clickOnChildren :: [UiElement] -> ClickHandler
clickOnChildren children position w = return w