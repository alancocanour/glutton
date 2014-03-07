module Hfeedreader.ItemStrategy where

import Text.Feed.Types (Item(..))

import Hfeedreader.Feed.Types (ItemState(..))

-- TODO come up with a better name than ItemStrategy
-- | A function that decides whether we want to store and display an item
type ItemStrategy = Maybe Item -- ^ The item as is currently exists in the feed
                   -> Maybe ItemState -- ^ The item as is currently exists on disk
                   -> Bool -- ^ Whether we want to store the item

inFeed :: ItemStrategy
inFeed Nothing _ = False
inFeed _ _ = True


-- TODO provide more strategies and maybe combinators to combine them with
