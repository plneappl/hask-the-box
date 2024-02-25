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
  name :: String,
  size :: Point,
  drawSelf :: World -> IO Picture,
  onClick :: ClickHandler
}

instance Show UiElement where
  show it = show (name it) ++ " | " ++ show (size it)

ignoreClicks :: ClickHandler
ignoreClicks _ w = return w