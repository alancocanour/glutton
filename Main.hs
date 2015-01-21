module Main where

import Glutton.Subscription.Updater
import Glutton.Gui
import Glutton.Config

main :: IO ()
main = do config <- getConfig
          (startGui, updateGui) <- createGui
          updater <- startUpdater (refreshTime config) (feeds config) updateGui
          startGui (port config) updater
          killUpdater updater
