{- |
Module      : Glutton.Config
Description : configuration types, file loading, and parsing
-}
module Glutton.Config
       ( Config (..)
       , ConfigHandle
       , newConfig
       , readConfig
       , loadConfig
       , modifyConfig
       , gluttonHome
       ) where

import System.FilePath ((</>))
import System.Environment (lookupEnv)
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Exception

data ConfigHandle = CH FilePath (TVar Config)

-- | User's Glutton configuration
data Config = Config { refreshTime :: Int -- ^ time in seconds between retrieving feeds
                     , port :: Int -- ^ the port to use for the GUI
                     , feeds :: [String] -- ^ URLs of feeds the user wants to view
                     } deriving (Show, Read)

defaultConfig :: Config
defaultConfig = Config 1200 9999 []

newConfig :: IO ConfigHandle
newConfig = do
  f <- gluttonConfigFile
  t <- newTVarIO defaultConfig
  return $ CH f t

loadConfig :: IO (Either IOException ConfigHandle)
loadConfig = do
  f <- gluttonConfigFile
  try $ do
    contents <- read <$> readFile f
    (CH f) <$> newTVarIO contents

readConfig :: ConfigHandle -> IO Config
readConfig (CH _ t) = readTVarIO t

modifyConfig :: ConfigHandle -> (Config -> Config) -> IO (Either IOException ())
modifyConfig (CH fp t) f = do
  c <- atomically $ modifyTVar' t f >> readTVar t
  try $ writeFile fp (show c)

-- | The FilePath where subscriptions are stored
gluttonHome :: IO FilePath
gluttonHome = do
  gluttonHomeEnv <- lookupEnv "GLUTTONHOME"
  homeEnv <- lookupEnv "HOME"
  let home = fromJust $ gluttonHomeEnv <|> homeEnv <|> Just "."
  return $ home </> ".glutton"

gluttonConfigFile :: IO FilePath
gluttonConfigFile = (</> "glutton.conf") <$> gluttonHome
