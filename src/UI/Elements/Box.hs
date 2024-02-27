module UI.Elements.Box
  ( hbox
  , vbox
  ) where

import Data.List(intersperse)
import Graphics.Gloss
import Main.World
import UI.UiElement
import UI.Util(render, isHit)
import Debug.Trace

hbox :: Point -> Float -> [UiElement] -> UiElement
hbox margin hseparator children = UiElement {
  name = "hbox " ++ (show children),
  size = bounds,
  drawSelf = layoutHorizontal bounds margin hseparator children,
  onClick = clickOnChildrenHorizontal hseparator children
} where 
  sizeX = sum $ intersperse hseparator $ map fst $ map size children
  sizeY = maximum $ map snd $ map size children
  bounds = (sizeX, sizeY)

vbox :: Point -> Float -> [UiElement] -> UiElement
vbox margin vseparator children = UiElement {
  name = "hbox " ++ (show children),
  size = bounds,
  drawSelf = layoutVertical bounds margin vseparator layoutChildren,
  onClick = clickOnChildrenVertical vseparator children
} where 
  sizeX = maximum $ map fst $ map size children
  sizeY = sum $ intersperse vseparator $ map snd $ map size children
  bounds = (sizeX, sizeY)
  -- since gloss's coordinate system has an inversed y-axis, we render children in reversed order 
  -- to restore sane coordinate system
  layoutChildren = reverse children

offsetsHorizontal :: Float -> [UiElement] -> [Float]
offsetsHorizontal hseparator children = scanl (\a b -> a + b + hseparator) 0 $ map fst $ map size children

offsetsVertical :: Float -> [UiElement] -> [Float]
offsetsVertical vseparator children = scanl (\a b -> a + b + vseparator) 0 $ map snd $ map size children

layoutHorizontal :: Point -> Point -> Float -> [UiElement] -> World -> IO Picture
layoutHorizontal _ _ _ [] _ = return Blank
layoutHorizontal (sizeX, sizeY) margin hseparator children w = do
  pics <- mapM (render w) children
  let offsets = offsetsHorizontal hseparator children
  return $ Pictures $ map (\(off, p) -> Translate off 0 p) $ zip offsets pics

layoutVertical :: Point -> Point -> Float -> [UiElement] -> World -> IO Picture
layoutVertical _ _ _ [] _ = return Blank
layoutVertical (sizeX, sizeY) margin vseparator children w = do
  pics <- mapM (render w) children
  let offsets = offsetsVertical vseparator children
  return $ Pictures $ map (\(off, p) -> Translate 0 off p) $ zip offsets pics

clickOnChildrenHorizontal :: Float -> [UiElement] -> ClickHandler
clickOnChildrenHorizontal hseparator children pos@(x, y) w = do
  let offsets = offsetsHorizontal hseparator children
  let matches = filter (\(xoff, c) -> isHit (xoff, 0) pos c) $ zip offsets children
  case matches of
    [] -> ignoreClicks pos w
    ((xoff, c):_) -> onClick c (x - xoff, y) w

clickOnChildrenVertical :: Float -> [UiElement] -> ClickHandler
clickOnChildrenVertical vseparator children pos@(x, y) w = do
  let offsets = offsetsVertical vseparator children
  let matches = filter (\(yoff, c) -> isHit (0, yoff) pos c) $ zip offsets children
  case matches of
    [] -> ignoreClicks pos w
    ((yoff, c):_) -> onClick c (x, y - yoff) w
