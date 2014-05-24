module Glutton.Config
       ( Config (..)
       , getConfig
       , gluttonHome
       ) where

import System.FilePath ((</>))
import System.Environment (lookupEnv)
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))

data Config = Config { refreshTime :: Int
                     , port :: Int
                     , feeds :: [String]
                     } deriving (Show, Read)

--TODO use a real configuration file format that doesn't require knowledge of Haskell
                                
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
