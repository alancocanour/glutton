{- |
Module      : Glutton.Config
Description : configuration types, file loading, and parsing
-}
module Glutton.Config
       ( Config (..)
       , getConfig
       , gluttonHome
       ) where

import System.FilePath ((</>))
import System.Environment (lookupEnv)
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))

-- | User's Glutton configuration
data Config = Config { refreshTime :: Int -- ^ time in seconds between retrieving feeds
                     , port :: Int -- ^ the port to use for the GUI
                     , feeds :: [String] -- ^ URLs of feeds the user wants to view
                     } deriving (Show, Read)

--TODO use a real configuration file format that doesn't require knowledge of Haskell

-- | Reads the user's configuration file of the disk or throws an error
getConfig :: IO Config
getConfig = do home <- gluttonHome
               configFile <- readFile $ home </> "glutton.conf"
               return $ read configFile

-- | The FilePath where subscriptions are stored
gluttonHome :: IO FilePath
gluttonHome = do gluttonHomeEnv <- lookupEnv "GLUTTONHOME"
                 homeEnv <- lookupEnv "HOME"
                 let home = fromJust $ gluttonHomeEnv <|> homeEnv <|> Just "."
                 return $ home </> ".glutton"
