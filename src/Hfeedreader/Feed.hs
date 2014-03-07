{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}
module Hfeedreader.Feed where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Maybe
import Data.SafeCopy
import Network.HTTP
import qualified Data.Map.Lazy as M 
import Text.Feed.Import
import Text.Feed.Query hiding (feedItems)
import Text.Feed.Types

import Hfeedreader.ItemStrategy
import Hfeedreader.Feed.Types
  
-- http://rss.slashdot.org/Slashdot/slashdot
getFeed :: String -> IO (Either String Feed)
getFeed url = (simpleHTTP (getRequest url)
              >>= getResponseBody
              >>= return . note "Failed to parse feed" . parseFeedString)
              `catch` \e -> return . Left $ show (e :: IOException)

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

mergeFeed :: ItemStrategy -> Feed -> FeedState -> FeedState
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

mergeItems :: ItemStrategy -> [Item] -> [ItemState] -> [ItemState]
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
                               
writeFeedState :: FeedState -> Update FeedState ()
writeFeedState = put

queryFeedState :: Query FeedState FeedState
queryFeedState = ask

openFeedState :: String -> IO (AcidState FeedState)
openFeedState url = openLocalStateFrom "~/.hfeedreader/" (newFeedState url)

$(deriveSafeCopy 0 'base ''ItemState_v0)
$(deriveSafeCopy 0 'base ''FeedState_v0)
$(makeAcidic ''FeedState_v0 ['writeFeedState, 'queryFeedState])

-- | Updates a feed subscription and maybe returns an error String
updateSub :: ItemStrategy -> AcidState FeedState -> IO (Maybe String)
updateSub s a = do fs <- query a QueryFeedState
                   f <- getFeed (feedUrl fs)
                   case f of
                     Left e -> return $ Just e
                     Right feed -> do update a (WriteFeedState (mergeFeed s feed fs))
                                      return Nothing

getSub :: AcidState FeedState -> IO (FeedState)
getSub a = query a QueryFeedState
