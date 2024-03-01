module UI.Elements.Button
  ( button
  ) where

import UI.Elements.Label
import UI.Elements.Rectangle
import UI.Elements.StackPane
import UI.UiElement

button :: String -> ClickHandler -> UiElement
button lbl _onClick =
  pane
    { onClick = _onClick
    , name = "btn (" ++ lbl ++ ")"
    }
 where
  labelText = label 20 lbl
  (lblWidth, lblHeight) = size labelText
  pane =
    stackPane
      [ labelText
      , rectangle (lblWidth + 10, lblHeight + 10)
      ]