module UI.Elements.Button 
  ( button
  ) where

import UI.UiElement
import Graphics.Gloss
import UI.Elements.Label
import UI.Elements.Rectangle
import UI.Elements.StackPane

button :: String -> ClickHandler -> UiElement
button lbl _onClick = stackPane 
  [ labelText
  , rectangle (lblWidth + 10, lblHeight + 10)
  ] where 
  labelText = label 20 lbl
  (lblWidth, lblHeight) = size labelText