module Glutton.Subscription.Updater
       ( Updater
       , Update(..)
       , startUpdater
       , killUpdater
       , getPort
       , addSubscription
       , removeSubscription
       ) where

import Control.Concurrent
import Control.Concurrent.Chan.Split
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Control.Monad
import Data.Functor
  
import Glutton.Subscription
import Glutton.ItemPredicate

data Updater = Updater { feeds :: TVar [(String, SubscriptionHandle)]
                       , port :: SendPort Update
                       , thread :: ThreadId
                       }

data Update = Update { url :: String
                     , error :: Maybe SomeException
                     }

startUpdater :: Int -- ^ time between updates in seconds
             -> [String] -- ^ Feed URLs to update
             -> IO Updater
startUpdater i fs = do
  sendPort <- newSendPort
  handles <- newTVarIO =<< mapM openHandle fs
  tid <- forkIO $ updateThread i handles sendPort
  return $ Updater handles sendPort tid
    where openHandle f = do h <- open f
                            return (f, h)
                  
updateThread :: Int -> TVar [(String, SubscriptionHandle)] -> SendPort Update -> IO ()
updateThread i handlesT p = do
  handles <- atomically $ readTVar handlesT
  forM_ handles $ uncurry $ sendUpdate p
  threadDelay $ i * 1000000
  updateThread i handlesT p

sendUpdate :: SendPort Update -> String -> SubscriptionHandle -> IO ()
sendUpdate p url_ handle = do
  err <- update inFeed handle
  send p $ Update url_ err
  
killUpdater :: Updater -> IO ()
killUpdater = killThread . thread

getPort :: Updater -> IO (ReceivePort Update)
getPort = listen . port

addSubscription :: Updater -> String -> IO ()
addSubscription u f = do
  handle <- open f
  atomically $ modifyTVar' (feeds u) ((f, handle) :)
  _ <- forkIO $ sendUpdate (port u) f handle
  return ()
          
removeSubscription :: Updater -> String -> IO ()
removeSubscription u f = do
  handle <- atomically $ do
    Just handle <- lookup f <$> readTVar (feeds u)
    modifyTVar' (feeds u) $ filter $ not . (== f) . fst
    return handle
  close handle
