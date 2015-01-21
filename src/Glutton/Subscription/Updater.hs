module Glutton.Subscription.Updater
       ( Updater
       , startUpdater
       , killUpdater
       , addSubscription
       , removeSubscription
       ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Functor
  
import Glutton.Subscription
import Glutton.ItemPredicate


data Updater = Updater
               { feeds :: TVar [(String, SubscriptionHandle)]
               , sendUpdate :: [SubscriptionHandle] -> IO ()
               , thread :: ThreadId
               }

startUpdater
  :: Int -- ^ time between updates in seconds
  -> [String] -- ^ Feed URLs to update
  -> ([SubscriptionHandle] -> IO ()) -- ^ An IO action to call when a SubscriptionHandle changes
  -> IO Updater
startUpdater i fs updateFunction = do
  handlesT <- newTVarIO []
  handles <- forM fs $ \f ->
    do h <- open f (updateFunction =<< return . map snd =<< readTVarIO handlesT)
       return (f, h)
  atomically $ writeTVar handlesT handles
  tid <- forkIO $ updateThread i handlesT updateFunction
  return $ Updater handlesT updateFunction tid

                  
updateThread :: Int -> TVar [(String, SubscriptionHandle)] -> ([SubscriptionHandle] -> IO ()) -> IO ()
updateThread i handlesT sendUpdates = do
  handles <- map snd <$> readTVarIO handlesT
  forM_ handles $ \h ->
    fetchSubscription (inFeed `orP` notP isRead) h
  threadDelay $ i * 1000000
  updateThread i handlesT sendUpdates

killUpdater :: Updater -> IO ()
killUpdater u = do
  killThread $ thread u
  handles <- map snd <$> readTVarIO (feeds u)
  mapM_ close handles

callSendUpdate :: Updater -> IO ()
callSendUpdate u = map snd <$> readTVarIO (feeds u) >>= sendUpdate u

addSubscription :: Updater -> String -> IO ()
addSubscription u f = do
  handle <- open f (callSendUpdate u)
  atomically $ modifyTVar' (feeds u) ((f, handle) :)
  callSendUpdate u
          
removeSubscription :: Updater -> String -> IO ()
removeSubscription u f = do
  handle <- atomically $ do
    Just handle <- lookup f <$> readTVar (feeds u)
    modifyTVar' (feeds u) $ filter $ (/= f) . fst
    return handle
  close handle
  callSendUpdate u
