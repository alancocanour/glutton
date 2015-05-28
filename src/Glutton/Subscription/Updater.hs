{- |
Module      : Glutton.Subscription.Updater
Description : a type for periodically updating @Subscription@s
-}
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

import qualified Glutton.Config as C
import Glutton.Subscription
import Glutton.ItemPredicate

-- | The Updater keeps track of the Subscriptions the user is
-- interested in, updates them periodically, and informs the user
-- interface when they change
data Updater = Updater
               { feeds :: TVar [(String, SubscriptionHandle)]
               , sendUpdate :: [SubscriptionHandle] -> IO ()
               , thread :: ThreadId
               , config :: C.ConfigHandle
               }

-- | Starts a background thread that updates feeds given in the
-- configuration at the interval given in the configuration and calls
-- a function when feeds are updated that can be used to update the
-- user interface
startUpdater ::
  C.ConfigHandle -- ^ A handle to access and modify user configuration
  -> ([SubscriptionHandle] -> IO ()) -- ^ An IO action to call when a SubscriptionHandle changes
  -> IO Updater
startUpdater configH updateFunction = do
  cfg <- C.readConfig configH
  handlesT <- newTVarIO []
  handles <- forM (C.feeds cfg) $ \f ->
    do h <- open f (updateFunction =<< return . map snd =<< readTVarIO handlesT)
       return (f, h)
  atomically $ writeTVar handlesT handles
  tid <- forkIO $ updateThread (C.refreshTime cfg) handlesT updateFunction
  return $ Updater handlesT updateFunction tid configH

-- | The background thread that does all the work
updateThread :: Int -> TVar [(String, SubscriptionHandle)] -> ([SubscriptionHandle] -> IO ()) -> IO ()
updateThread i handlesT sendUpdates = do
  handles <- map snd <$> readTVarIO handlesT
  forM_ handles $ \h ->
    fetchSubscription (inFeed `orP` notP isRead) h
  threadDelay $ i * 1000000
  updateThread i handlesT sendUpdates

-- | Stops the Updater's background thread and closes all of its
-- opened SubscriptionHandles
killUpdater :: Updater -> IO ()
killUpdater u = do
  killThread $ thread u
  handles <- map snd <$> readTVarIO (feeds u)
  mapM_ close handles

-- | The function for updating the user interface with all of the current feeds
callSendUpdate :: Updater -> IO ()
callSendUpdate u = map snd <$> readTVarIO (feeds u) >>= sendUpdate u

-- | Add a Subscription to the updater to have it automatically
-- updated and added to the user interface
addSubscription :: Updater -> String -> IO ()
addSubscription u f = do
  handle <- open f (callSendUpdate u)
  atomically $ modifyTVar' (feeds u) ((f, handle) :)
  _ <- C.modifyConfig (config u) (\c -> c { C.feeds = f : C.feeds c})
  callSendUpdate u

-- | Remove a Subscription from the list of Subscriptions to be
-- updated and remove it from the user interface
removeSubscription :: Updater -> String -> IO ()
removeSubscription u f = do
  handle <- atomically $ do
    Just handle <- lookup f <$> readTVar (feeds u)
    modifyTVar' (feeds u) $ filter $ (/= f) . fst
    return handle
  close handle
  _ <- C.modifyConfig (config u) (\c -> c { C.feeds = filter (/= f) (C.feeds c)})
  callSendUpdate u
