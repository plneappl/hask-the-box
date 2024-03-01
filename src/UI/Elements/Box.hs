{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module UI.Elements.Box
  ( hbox
  , vbox
  ) where

import Data.List (intersperse)
import Graphics.Gloss
import Main.World
import UI.UiElement
import UI.Util (isHit, render)

hbox :: Point -> Float -> [UiElement] -> UiElement
hbox margin@(marginX, marginY) hseparator children =
  UiElement
    { name = "hbox " ++ show children
    , size = bounds
    , drawSelf = layoutHorizontal bounds margin hseparator children
    , onClick = clickOnChildrenHorizontal margin hseparator children
    }
 where
  sizeX = sum $ intersperse hseparator $ map (fst . size) children
  sizeY = maximum $ map (snd . size) children
  bounds = (marginX + sizeX, marginY + sizeY)

vbox :: Point -> Float -> [UiElement] -> UiElement
vbox margin@(marginX, marginY) vseparator children =
  UiElement
    { name = "hbox " ++ show children
    , size = bounds
    , drawSelf = layoutVertical bounds margin vseparator layoutChildren
    , onClick = clickOnChildrenVertical margin vseparator children
    }
 where
  sizeX = maximum $ map (fst . size) children
  sizeY = sum $ intersperse vseparator $ map (snd . size) children
  bounds = (marginX + sizeX, marginY + sizeY)
  -- since gloss's coordinate system has an inversed y-axis, we render children in reversed order
  -- to restore sane coordinate system
  layoutChildren = reverse children

offsetsHorizontal :: Float -> [UiElement] -> [Float]
offsetsHorizontal hseparator children = scanl (\a b -> a + b + hseparator) 0 $ map (fst . size) children

offsetsVertical :: Float -> [UiElement] -> [Float]
offsetsVertical vseparator children = scanl (\a b -> a + b + vseparator) 0 $ map (snd . size) children

layoutHorizontal :: Point -> Point -> Float -> [UiElement] -> World -> IO Picture
layoutHorizontal _ _ _ [] _ = return Blank
layoutHorizontal (sizeX, sizeY) (marginX, marginY) hseparator children w = do
  pics <- mapM (render w) children
  let offsets = offsetsHorizontal hseparator children
  return $ Translate marginX marginY $ Pictures $ zipWith (\ off p -> Translate off 0 p) offsets pics

layoutVertical :: Point -> Point -> Float -> [UiElement] -> World -> IO Picture
layoutVertical _ _ _ [] _ = return Blank
layoutVertical (sizeX, sizeY) (marginX, marginY) vseparator children w = do
  pics <- mapM (render w) children
  let offsets = offsetsVertical vseparator children
  return $ Translate marginX marginY $ Pictures $ zipWith (\ off p -> Translate 0 off p) offsets pics

clickOnChildrenHorizontal :: Point -> Float -> [UiElement] -> ClickHandler
clickOnChildrenHorizontal (marginX, marginY) hseparator children (_x, _y) w = do
  let x = _x - marginX
  let y = _y - marginY
  let pos = (x, y)
  let offsets = offsetsHorizontal hseparator children
  let matches = filter (\(xoff, c) -> isHit (xoff, 0) pos c) $ zip offsets children
  case matches of
    [] -> ignoreClicks pos w
    ((xoff, c) : _) -> onClick c (x - xoff, y) w

clickOnChildrenVertical :: Point -> Float -> [UiElement] -> ClickHandler
clickOnChildrenVertical (marginX, marginY) vseparator children (_x, _y) w = do
  let x = _x - marginX
  let y = _y - marginY
  let pos = (x, y)
  let offsets = offsetsVertical vseparator children
  let matches = filter (\(yoff, c) -> isHit (0, yoff) pos c) $ zip offsets children
  case matches of
    [] -> ignoreClicks pos w
    ((yoff, c) : _) -> onClick c (x, y - yoff) w
