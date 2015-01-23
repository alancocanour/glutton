{- |
Module      : Glutton.Subscription
Description : retrieving, merging, and storing subscriptions/feeds
-}
module Glutton.Subscription (
  open,
  close,
  get,
  fetchSubscription,
  modify,
  modifyAndRead,
  SubscriptionHandle,
  module Glutton.Subscription.Types
  ) where

import Control.Concurrent
import Control.Exception (try, SomeException(..), ErrorCall(..))
import Control.Monad
import Data.Acid hiding (update)
import Data.Maybe
import Data.Monoid
import Data.Acid.Local
import Network.HTTP (simpleHTTP, getResponseBody, getRequest, urlEncode)
import qualified Data.Map.Lazy as M 
import qualified Data.Acid as A
import System.Directory
import System.FilePath ((</>))
import Text.Feed.Import
import Text.Feed.Query hiding (feedItems)
import Text.Feed.Types

import Glutton.Config (gluttonHome)
import Glutton.ItemPredicate
import Glutton.Subscription.Types

-- | A wrapper around a 'Subscription' that allows safe modifications
data SubscriptionHandle = SH (AcidState Subscription) (IO ())

-- | Retrieves a 'Feed' from the remote server
getFeed :: String -> IO (Either SomeException Feed)
getFeed url = do feedString <- try $ simpleHTTP (getRequest url) >>= getResponseBody
                 return $ feedString >>= parseFeed
  where parseError = Left $ SomeException $ ErrorCall "Failed to parse feed"
        parseFeed = maybe parseError Right . parseFeedString
--TODO make a custom exception type instead of using ErrorCall

-- | Merges the 'Feed' from a remote server into the 'Subscription' on
-- the local disk keeping only the items for which the 'ItemPredicate'
-- is 'True'
mergeFeed :: ItemPredicate -> Feed -> Subscription -> Subscription
mergeFeed s f fs = fs { feedTitle = getFeedTitle f
                      , feedAuthor = getFeedAuthor f
                      , feedHome = getFeedHome f
                      , feedHtml = getFeedHTML f
                      , feedDescription = getFeedDescription f
                      , feedPubDate = getFeedPubDate f
                      , feedLastUpdate = getFeedLastUpdate f
                      , feedDate = getFeedDate f
                      , feedLogoLink = getFeedLogoLink f
                      , feedLanguage = getFeedLanguage f
                      , feedCategories = getFeedCategories f
                      , feedGenerator = getFeedGenerator f
                      , feedItems = mergeItems s (getFeedItems f) (feedItems fs)
                      , feedLastError = Nothing
                      }

-- | Merge 'Item' in the remote 'Feed' into the existing 'ItemState's
-- and keeps 'ItemState's for with the 'ItemPredicate' is 'True'
mergeItems :: ItemPredicate -> [Item] -> [ItemState] -> [ItemState]
mergeItems f i is =
  let iM =  M.fromList $ map (\a -> (getId a,  (Just a, Nothing))) i
      isM = M.fromList $ map (\a -> (itemId a, (Nothing, Just a))) is
      matchedItemsM = M.unionWith combine iM isM
      filteredItemsM = M.filter (uncurry f) matchedItemsM
      itemPairs = map snd $ M.toList filteredItemsM
      combine (a, Nothing) (Nothing, b) = (a,b)
      combine _ _ = undefined --This should never happen
  in map (uncurry mergeItem) itemPairs

mergeItem :: Maybe Item -> Maybe ItemState -> ItemState
mergeItem Nothing (Just is)= is
mergeItem mi@(Just i) Nothing = mergeItem mi $ Just (newItemState (getId i))
mergeItem (Just i) (Just is) =
  is { itemTitle = getItemTitle i
     , itemLink = getItemLink i
     , itemPublishDateString = getItemPublishDateString i
     , itemDate = getItemDate i
     , itemAuthor = getItemAuthor i
     , itemCommentLink = getItemCommentLink i
     , itemEnclosure = getItemEnclosure i
     , itemFeedLink = getItemFeedLink i
     , itemCategories = getItemCategories i
     , itemRights = getItemRights i
     , itemSummary = getItemSummary i
     , itemDescription = getItemDescription i
     }
mergeItem Nothing Nothing = error "Can't merge item Nothing with item Nothing"

getId :: Item -> String
getId i = let id_ = First $ fmap snd $ getItemId i
              title = First $ getItemTitle i
          in fromMaybe (error "Item lacks an id and title") $ getFirst $ id_ <> title

-- | The folder where a subscription is saved on disk
subscriptionFolder :: String -> IO FilePath
subscriptionFolder url = do
  home <- gluttonHome
  return $ home </> urlEncode url

-- | Opens a 'SubscriptionHandle' for the specified URL.
open
  :: String -- ^ The URL of the RSS/ATOM feed
  -> IO () -- ^ IO action to perform whenever the subscription is modified
  -> IO SubscriptionHandle
open url f = do
  folder <- subscriptionFolder url
  s <- openLocalStateFrom folder $ newSubscription url
  return $ SH s f

-- | Closes the 'SubscriptionHandle'
close :: SubscriptionHandle -> IO ()
close (SH a _) = createCheckpointAndClose a

-- | Retrieves subscription contents from remote server and updates the 'SubscriptionHandle'
fetchSubscription :: ItemPredicate -> SubscriptionHandle -> IO ()
fetchSubscription p sh = do
  f <- getFeed =<< fmap feedUrl (get sh)
  case f of
    Left e -> modify sh (\s -> s {feedLastError = Just (show e)})
    Right feed -> modify sh (mergeFeed p feed)
  cleanup sh

-- | Gets a 'Subscription' from a 'SubscriptionHandle'
get :: SubscriptionHandle -> IO Subscription
get (SH a _) = query a QuerySubscription

-- | Modifies a 'Subscription' associated with a 'SubscriptionHandle' and extracts some information from it at the same time
modifyAndRead :: SubscriptionHandle -> (Subscription -> (Subscription, b)) -> IO b
modifyAndRead sh@(SH a sendUpdate) f = do
  s <- get sh
  let (s', b) = f s
  A.update a (WriteSubscription s')
  sendUpdate
  return b

-- | Modifies a 'Subscription' associated with a 'SubscriptionHandle'
modify :: SubscriptionHandle -> (Subscription -> Subscription) -> IO ()
modify sh f = modifyAndRead sh (\a -> (f a, ()))

-- | Removes old acid state checkpoints and event logs
cleanup :: SubscriptionHandle -> IO ()
cleanup sh@(SH a _) = void $ forkIO $ do
  createCheckpoint a
  createArchive a
  url <- fmap feedUrl $ get sh
  removeDirectoryRecursive =<< fmap (</> "Archive") (subscriptionFolder url)
