module Glutton.Subscription.Updater
       ( Updater
       , startUpdater
       , killUpdater
       , getFeeds
       , getPort
       , addSubscription
       , removeSubscription
       ) where

import Control.Concurrent
import Control.Concurrent.Chan.Split
import Control.Concurrent.STM
import Control.Monad
import Data.Functor
  
import Glutton.Subscription
import Glutton.ItemPredicate

data Updater = Updater { feeds :: TVar [(String, SubscriptionHandle)]
                       , port :: SendPort [SubscriptionHandle]
                       , thread :: ThreadId
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
                  
updateThread :: Int -> TVar [(String, SubscriptionHandle)] -> SendPort [SubscriptionHandle] -> IO ()
updateThread i handlesT p = do
  handles <- map snd <$> readTVarIO handlesT
  forM_ handles $ \h -> do
    update (inFeed `orP` notP isRead) h
    sendUpdate' p handlesT
  threadDelay $ i * 1000000
  updateThread i handlesT p

sendUpdate :: Updater -> IO ()
sendUpdate u = sendUpdate' (port u) (feeds u)

sendUpdate' :: SendPort [SubscriptionHandle] -> TVar [(String, SubscriptionHandle)] -> IO ()
sendUpdate' p t = send p =<< map snd <$> readTVarIO t

killUpdater :: Updater -> IO ()
killUpdater u = do
  killThread $ thread u
  handles <- map snd <$> readTVarIO (feeds u)
  mapM_ close handles

getFeeds :: Updater -> IO [SubscriptionHandle]
getFeeds u = map snd <$> readTVarIO (feeds u)
              
getPort :: Updater -> IO (ReceivePort [SubscriptionHandle])
getPort = listen . port

addSubscription :: Updater -> String -> IO ()
addSubscription u f = do
  handle <- open f
  atomically $ modifyTVar' (feeds u) ((f, handle) :)
  sendUpdate u
          
removeSubscription :: Updater -> String -> IO ()
removeSubscription u f = do
  handle <- atomically $ do
    Just handle <- lookup f <$> readTVar (feeds u)
    modifyTVar' (feeds u) $ filter $ (/= f) . fst
    return handle
  close handle
  sendUpdate u
