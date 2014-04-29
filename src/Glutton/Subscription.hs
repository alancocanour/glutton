{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}
module Glutton.Subscription where

import Control.Exception (try, SomeException(..), ErrorCall(..))
import Control.Error
import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Data.Acid
import Data.Maybe
import Data.SafeCopy
import Network.HTTP
import qualified Data.Map.Lazy as M 
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Feed.Import
import Text.Feed.Query hiding (feedItems)
import Text.Feed.Types

import Glutton.ItemPredicate
import Glutton.Subscription.Types
  
getFeed :: String -> IO (Either SomeException Feed)
getFeed url = runEitherT $
              EitherT (try (simpleHTTP (getRequest url)))
              >>= syncIO . getResponseBody
              >>= hoistEither . note (SomeException (ErrorCall "Failed to parse feed")) . parseFeedString

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
                      }

mergeItems :: ItemPredicate -> [Item] -> [ItemState] -> [ItemState]
mergeItems f i is =
  let iM =  M.fromList $ map (\a -> (getId a,  (Just a, Nothing))) i
      isM = M.fromList $ map (\a -> (itemId a, (Nothing, Just a))) is
      matchedItemsM = M.unionWith combine iM isM
      filteredItemsM = M.filter (\(a,b) -> f a b) matchedItemsM
      itemPairs = map snd $ M.toList filteredItemsM
      combine (a, Nothing) (Nothing, b) = (a,b)
  in map (\(a, b) -> mergeItem a b) itemPairs

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

getId = snd . fromMaybe (error "Item lacks an id") . getItemId
                               
writeSubscription :: Subscription -> Update Subscription ()
writeSubscription = put

querySubscription :: Query Subscription Subscription
querySubscription = ask

openSubscription :: String -> IO (AcidState Subscription)
openSubscription url = do home <- gluttonHome
                          let subscriptionFolder = home </> (urlEncode url)
                          openLocalStateFrom home (newSubscription url)

gluttonHome :: IO FilePath
gluttonHome = do gluttonHomeEnv <- lookupEnv "GLUTTONHOME"
                 homeEnv <- lookupEnv "HOME"
                 let home = head $ catMaybes [gluttonHomeEnv, homeEnv, Just "."]
                     gluttonHome = home </> ".glutton"
                 return gluttonHome

$(deriveSafeCopy 0 'base ''ItemState)
$(deriveSafeCopy 0 'base ''Subscription)
$(makeAcidic ''Subscription ['writeSubscription, 'querySubscription])

-- | Updates a feed subscription and maybe returns an exception if the update fails
updateSubscription :: ItemPredicate -> AcidState Subscription -> IO (Maybe SomeException)
updateSubscription s a = do fs <- query a QuerySubscription
                            f <- getFeed (feedUrl fs)
                            case f of
                              Left e -> return $ Just e
                              Right feed -> do update a (WriteSubscription (mergeFeed s feed fs))
                                               return Nothing

getSubscription :: AcidState Subscription -> IO Subscription
getSubscription a = query a QuerySubscription
