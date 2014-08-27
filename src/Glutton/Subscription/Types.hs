{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Glutton.Subscription.Types where

import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Data.Acid
import Data.SafeCopy
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
    feedUrl :: String,
    feedLastError :: Maybe String
    } deriving (Typeable, Show)

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
  feedUrl = url,
  feedLastError = Nothing
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
    } deriving (Show)

newItemState :: String -> ItemState
newItemState _id =
  ItemState { 
    itemTitle = Nothing,
    itemLink = Nothing,
    itemPublishDateString = Nothing,
    itemDate = Nothing,
    itemAuthor = Nothing,
    itemCommentLink = Nothing,
    itemEnclosure = Nothing,
    itemFeedLink = Nothing,
    itemId = _id,
    itemCategories = [],
    itemRights = Nothing,
    itemSummary = Nothing,
    itemDescription = Nothing,
    itemRead = False
    }

writeSubscription :: Subscription -> Update Subscription ()
writeSubscription = put

querySubscription :: Query Subscription Subscription
querySubscription = ask

$(deriveSafeCopy 0 'base ''ItemState) --Move these to Subscription.Types to fix orphan instances
$(deriveSafeCopy 0 'base ''Subscription)
$(makeAcidic ''Subscription ['writeSubscription, 'querySubscription])
