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

isRead :: ItemPredicate
isRead _ (Just i) | itemRead i = True
isRead _ _ = False

andP :: ItemPredicate -> ItemPredicate -> ItemPredicate
f `andP` g = \i is -> f i is && g i is

orP :: ItemPredicate -> ItemPredicate -> ItemPredicate
f `orP` g = \i is -> f i is || g i is

notP :: ItemPredicate -> ItemPredicate
notP f i is = not $ f i is
