module Glutton.ItemStrategy where

import Text.Feed.Types (Item(..))

import Glutton.Feed.Types (ItemState(..), ItemState_v0(..))

-- TODO rename to ItemPredicate?
-- | A function that decides whether we want to store and display an item
type ItemStrategy = Maybe Item -- ^ The item as is currently exists in the feed
                   -> Maybe ItemState -- ^ The item as is currently exists on disk
                   -> Bool -- ^ Whether we want to store the item

inFeed :: ItemStrategy
inFeed (Just _) _ = True
inFeed _ _ = False

read :: ItemStrategy
read _ (Just i) | itemRead i = True
read _ _ = False

-- TODO provide more strategies and maybe combinators to combine them with
