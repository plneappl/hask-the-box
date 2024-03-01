module UI.Elements.StackPane
  ( stackPane
  ) where

import Graphics.Gloss (Picture (..), Point)
import Main.World
import UI.UiElement
import UI.Util (isHit, render)

stackPane :: [UiElement] -> UiElement
stackPane _children =
  UiElement
    { name = "stackPane " ++ show children
    , size = paneSize
    , drawSelf = drawStackPane paneSize children
    , onClick = onClickStackPane paneSize children
    }
 where
  sizeX :: Float
  sizeY :: Float
  sizeX = maximum $ map (fst . size) children
  sizeY = maximum $ map (snd . size) children
  paneSize = (sizeX, sizeY)
  children = reverse _children

drawStackPane :: Point -> [UiElement] -> World -> IO Picture
drawStackPane size children world = do
  pics <- mapM renderCentered children
  return $ Pictures pics
 where
  renderCentered :: UiElement -> IO Picture
  renderCentered c = do
    let (xoff, yoff) = centerChild size c
    p <- render world c
    return $ Translate xoff yoff p

centerChild :: Point -> UiElement -> Point
centerChild (width, height) c =
  let
    (w, h) = size c
   in
    ((width - w) / 2, (height - h) / 2)

onClickStackPane :: Point -> [UiElement] -> ClickHandler
onClickStackPane size children pos =
  let
    matches = filter (\c -> isHit (centerChild size c) pos c) children
   in
    case matches of
      [] -> return
      (c : _) -> onClick c pos