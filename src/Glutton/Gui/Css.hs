{-# LANGUAGE OverloadedStrings #-}
module Glutton.Gui.Css (stylesheet) where

import Clay
import Clay.Display as D
import Data.Text.Lazy

stylesheet :: String
stylesheet = unpack $ render $ do
  body ? do backgroundColor black
            color white
            display D.table
            height (pct 100)
  "#sidebar" ? do margin (px 0) (px 10) (px 0) (px 0)
                  width $ px 300
                  height $ pct 100
                  display tableCell
  "#content" ? do noMargin
                  height $ pct 100
                  display tableCell
  ".unreadCount" ? float floatRight
  where noMargin = margin (px 0) (px 0) (px 0) (px 0)
