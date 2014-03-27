module Glutton.ItemPredicate where

import Text.Feed.Types (Item(..))

import Glutton.Subscription.Types (ItemState(..))

-- | A function that decides whether we want to store and display an item
type ItemPredicate = Maybe Item -- ^ The item as is currently exists in the feed
                     -> Maybe ItemState -- ^ The item as is currently exists on disk
                     -> Bool -- ^ Whether we want to store the item

inFeed :: ItemPredicate
inFeed (Just _) _ = True
inFeed _ _ = False

read :: ItemPredicate
read _ (Just i) | itemRead i = True
read _ _ = False

-- TODO provide more strategies and maybe combinators to combine them with
