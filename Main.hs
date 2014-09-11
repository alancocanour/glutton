module Main where

import Glutton.Subscription.Updater
import Glutton.Gui
import Glutton.Config

main :: IO ()
main = do config <- getConfig
          updater <- startUpdater (refreshTime config) (feeds config)
          startGui (port config) updater
