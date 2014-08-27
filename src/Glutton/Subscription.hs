module Glutton.Subscription (
  open,
  close,
  get,
  update,
  modify,
  modifyAndRead,
  SubscriptionHandle,
  module Glutton.Subscription.Types
  ) where

import Control.Exception (try, SomeException(..), ErrorCall(..))
import Data.Acid hiding (update)
import qualified Data.Acid as A
import Data.Maybe
import Data.Monoid
import Network.HTTP (simpleHTTP, getResponseBody, getRequest, urlEncode)
import qualified Data.Map.Lazy as M 
import System.FilePath ((</>))
import Text.Feed.Import
import Text.Feed.Query hiding (feedItems)
import Text.Feed.Types

import Glutton.Config (gluttonHome)
import Glutton.ItemPredicate
import Glutton.Subscription.Types
  
getFeed :: String -> IO (Either SomeException Feed)
getFeed url = do feedString <- try $ simpleHTTP (getRequest url) >>= getResponseBody
                 return $ feedString >>= parseFeed
  where parseError = Left $ SomeException $ ErrorCall "Failed to parse feed"
        parseFeed = maybe parseError Right . parseFeedString
--TODO make a custom exception type instead of using ErrorCall

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

newtype SubscriptionHandle = SH (AcidState Subscription)

-- | Opens a SubscriptionHandle for the specified URL.
open :: String -> IO SubscriptionHandle
open url = do home <- gluttonHome
              let subscriptionFolder = home </> urlEncode url
              s <- openLocalStateFrom subscriptionFolder $ newSubscription url
              return $ SH s

-- | Closes the SubscriptionHandle
close :: SubscriptionHandle -> IO ()
close (SH a) = closeAcidState a

-- | Updates a feed subscription
update :: ItemPredicate -> SubscriptionHandle -> IO ()
update p sh = do s <- get sh
                 f <- getFeed (feedUrl s)
                 case f of
                   Left e -> put sh s {feedLastError = Just (show e)}
                   Right feed -> put sh (mergeFeed p feed s)

-- | Gets a Subscription from a SubscriptionHandle
get :: SubscriptionHandle -> IO Subscription
get (SH a) = query a QuerySubscription

-- | Writes a Subscription to a SubscriptionHandle
put :: SubscriptionHandle -> Subscription -> IO ()
put (SH a) s = A.update a (WriteSubscription s)

-- | Modifies a Subscription associated with a SubscriptionHandle and extracts some information from it at the same time
modifyAndRead :: SubscriptionHandle -> (Subscription -> (Subscription, b)) -> IO b
modifyAndRead (SH a) f = do
  fs <- query a QuerySubscription
  let (fs', b) = f fs
  A.update a (WriteSubscription fs')
  return b

-- | Modifies a Subscription associated with a SubscriptionHandle
modify :: SubscriptionHandle -> (Subscription -> Subscription) -> IO ()
modify sh f = modifyAndRead sh (\a -> (f a, ()))
