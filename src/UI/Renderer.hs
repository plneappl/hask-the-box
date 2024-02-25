module UI.Renderer
  ( renderWorld
  ) where

import Graphics.Gloss
import Main.World
import UI.Elements.Box
import UI.Elements.Button
import UI.Elements.LED
import UI.UiElement
import UI.Util(render, scaleUnif)
import Debug.Trace

renderWorld :: Point -> World -> IO Picture
renderWorld screen w = renderRoot screen w $ 
  vbox (0, 0) 15 
    [ button "Shuffle1" ignoreClicks
    , hbox (0, 0) 10 
      [ slottedLed 0
      , slottedLed 1
      , slottedLed 2
      , slottedLed 3
      ]
    , hbox (0, 0) 10 
      [ slottedLed 0
      , slottedLed 1
      , slottedLed 2
      , slottedLed 3
      ]
    , button "Shuffle2" ignoreClicks
    ]

renderRoot :: Point -> World -> UiElement -> IO Picture
renderRoot (screenX, screenY) w it = do
  let (sizeX, sizeY) = size it
  p <- render w it
  let anchorTopLeft = Translate (- screenX / 2) (screenY / 2 - sizeY) p
  return anchorTopLeft

-- for debugging
renderWorld1 (screenX, screenY) _ = return $ Pictures
  [ Translate 0 0 $ scaleUnif 0.1 $ Text "0, 0"
  , Translate 80 0 $ scaleUnif 0.1 $ Text "80, 0"
  , Translate 160 0 $ scaleUnif 0.1 $ Text "160, 0"
  , Translate (screenX / 2) 0 $ scaleUnif 0.1 $ Text "screenX, 0"
  , Translate (-80) 0 $ scaleUnif 0.1 $ Text "-80, 0"
  , Translate (-160) 0 $ scaleUnif 0.1 $ Text "-160, 0"
  , Translate (-screenX / 2) 0 $ scaleUnif 0.1 $ Text "-screenX, 0"
  , Translate 0 80 $ scaleUnif 0.1 $ Text "0, 80"
  , Translate 0 160 $ scaleUnif 0.1 $ Text "0, 160"
  , Translate 0 (screenY / 2) $ scaleUnif 0.1 $ Text "0, screenY"
  , Translate 0 (-80) $ scaleUnif 0.1 $ Text "0, -80"
  , Translate 0 (-160) $ scaleUnif 0.1 $ Text "0, -160"
  , Translate 0 (-screenY / 2) $ scaleUnif 0.1 $ Text "0, -screenY"
  ]
renderWorld2 screen w = do
  p <- renderRoot screen w $ button "Shuffle" ignoreClicks
  return $ trace (show p) p
renderWorld3 screen w = renderRoot screen w $ vbox (0, 0) 15 
  [ button "Shuffle" ignoreClicks
  , button "Shuffle" ignoreClicks
  , button "Shuffle" ignoreClicks
  , button "Shuffle" ignoreClicks
  ]