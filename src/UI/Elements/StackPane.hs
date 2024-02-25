module UI.Elements.StackPane 
  ( stackPane
  ) where

import Graphics.Gloss(Picture(..), Point)
import Main.World
import UI.UiElement
import UI.Util(render)

stackPane :: [UiElement] -> UiElement
stackPane children = UiElement {
  name = "stackPane " ++ (show children),
  size = paneSize,
  drawSelf = drawStackPane paneSize children,
  onClick = onClickStackPane children
} where
  sizeX :: Float
  sizeY :: Float
  sizeX = maximum $ map fst $ map size children
  sizeY = maximum $ map snd $ map size children
  paneSize = (sizeX, sizeY)

drawStackPane :: Point -> [UiElement] -> World -> IO Picture
drawStackPane (width, height) children world = do 
  pics <- mapM renderCentered children
  return $ Pictures pics
  where
    renderCentered :: UiElement -> IO Picture
    renderCentered c = do
      let (w, h) = size c
      p <- render world c
      return $ Translate ((width - w) / 2) ((height - h) / 2) p

onClickStackPane :: [UiElement] -> ClickHandler
onClickStackPane children pos w = return w