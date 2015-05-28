module Main where

import Glutton.Subscription.Updater
import Glutton.Gui
import Glutton.Config

main :: IO ()
main = do
  -- Load user configuration or create a new one
  configE <- loadConfig
  configH <- case configE of
    Left e -> putStrLn (show e) >> newConfig
    Right c -> return c
  config <- readConfig configH

  -- Start GUI and Updater
  (startGui, updateGui) <- createGui
  updater <- startUpdater configH updateGui
  startGui (port config) updater

  -- Clean up
  killUpdater updater
