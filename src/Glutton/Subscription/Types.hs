{-# LANGUAGE DeriveDataTypeable #-}
module Glutton.Subscription.Types where

import Data.Typeable

-- | A Subscription is the state of a Feed as it is stored on our local machine
data Subscription =
  Subscription {
    feedTitle :: String,
    feedAuthor :: Maybe String,
    feedHome :: Maybe String,
    feedHtml :: Maybe String,
    feedDescription :: Maybe String,
    feedPubDate :: Maybe String,
    feedLastUpdate :: Maybe String,
    feedDate :: Maybe String,
    feedLogoLink :: Maybe String,
    feedLanguage :: Maybe String,
    feedCategories :: [(String, Maybe String)],
    feedGenerator :: Maybe String,
    feedItems :: [ItemState],
    feedUrl :: String
    } deriving (Typeable)

newSubscription :: String -> Subscription
newSubscription url = Subscription {
  feedTitle = url,
  feedAuthor = Nothing,
  feedHome = Nothing,
  feedHtml = Nothing,
  feedDescription = Nothing,
  feedPubDate = Nothing,
  feedLastUpdate = Nothing,
  feedDate = Nothing,
  feedLogoLink = Nothing,
  feedLanguage = Nothing,
  feedCategories = [],
  feedGenerator = Nothing,
  feedItems = [],
  feedUrl = url
  }

data ItemState =
  ItemState {
    itemTitle :: Maybe String,
    itemLink :: Maybe String,
    --itemPublishDate :: ParseTime t => Maybe (Maybe t),
    itemPublishDateString :: Maybe String,
    itemDate :: Maybe String,
    itemAuthor :: Maybe String,
    itemCommentLink :: Maybe String,
    itemEnclosure :: Maybe (String, Maybe String, Maybe Integer),
    itemFeedLink :: Maybe String,
    itemId :: String,
    itemCategories :: [String],
    itemRights :: Maybe String,
    itemSummary :: Maybe String,
    itemDescription :: Maybe String,
    itemRead :: Bool
    }

newItemState :: String -> ItemState
newItemState id =
  ItemState { 
    itemTitle = Nothing,
    itemLink = Nothing,
    itemPublishDateString = Nothing,
    itemDate = Nothing,
    itemAuthor = Nothing,
    itemCommentLink = Nothing,
    itemEnclosure = Nothing,
    itemFeedLink = Nothing,
    itemId = id,
    itemCategories = [],
    itemRights = Nothing,
    itemSummary = Nothing,
    itemDescription = Nothing,
    itemRead = False
    }
