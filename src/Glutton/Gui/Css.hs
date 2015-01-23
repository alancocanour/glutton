{- |
Module      : Glutton.Gui.Css
Description : Cascading Style Sheets for the GUI
-}
{-# LANGUAGE OverloadedStrings #-}
module Glutton.Gui.Css (stylesheet) where

import Clay hiding (b)
import Clay.Display as D
import Data.Text.Lazy

stylesheet :: String
stylesheet = unpack $ render $ do
  body ? do
    backgroundColor black
    color white
    display D.table
    height (pct 100)
    "a:link" ? color "#DDDDDD"
    "a:visited" ? color "#666666"
    "a:hover" ? color "#FFFFFF"
  "#sidebar" ? do
    marginPx 20 20 0 0
    paddingPx 5 5 5 5
    width $ px 300
    height $ pct 100
    display tableCell
  "#content" ? do
    marginPx 20 20 0 0
    paddingPx 5 5 5 5
    height $ pct 100
    display tableCell
  ".feedSelector" ? do
    clickable
    fontFamily ["Helvetica"] [sansSerif]
  ".unreadCount" ? float floatRight
  ".activeFeedTitle" ? do
    display D.block
    fontFamily ["Helvetica"] [sansSerif]
    fontSizeCustom larger
    fontWeight bolder
    marginPx 10 10 10 20
  ".button" ? do
    border solid (px 3) "#666666"
    borderRadius (px 10) (px 10) (px 10) (px 10)
    fontFamily ["Helvetica"] [sansSerif]
    fontWeight bold
    fontSizeCustom large
    display D.block
    float floatLeft
    paddingPx 2 2 2 2
    color black
    backgroundColor "#DDDDDD"
    clickable
  ".feedItem" ? do
    clear both
    paddingPx 0 0 2 2
  ".clickable" ? clickable
  where marginPx l r t b = margin (px l) (px r) (px t) (px b)
        paddingPx l r t b = padding (px l) (px r) (px t) (px b)
        clickable = "cursor" -: "pointer"
