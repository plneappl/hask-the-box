{-# LANGUAGE PatternSynonyms, ExplicitNamespaces #-}

module UI.UiElement
  ( UiElement(..)
  , ignoreClicks
  , type ClickHandler
  ) where

import Main.World
import Graphics.Gloss

type ClickHandler = Point -> World -> IO World

data UiElement = UiElement {
  size :: Point,
  drawSelf :: World -> IO Picture,
  onClick :: ClickHandler
}

ignoreClicks :: ClickHandler
ignoreClicks _ w = return w