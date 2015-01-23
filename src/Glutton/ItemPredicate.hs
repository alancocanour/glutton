{- |
Module      : Glutton.ItemPredicate
Description : predicates on @Item@ and @ItemState@
-}
module Glutton.ItemPredicate where

import Text.Feed.Types (Item(..))

import Glutton.Subscription.Types (ItemState(..))

-- | A function that decides whether we want to store and display an item
type ItemPredicate = Maybe Item -- ^ The item as is currently exists in the feed
                     -> Maybe ItemState -- ^ The item as is currently exists on disk
                     -> Bool -- ^ Whether we want to store the item

-- | True when the item is present in the remote feed
inFeed :: ItemPredicate
inFeed (Just _) _ = True
inFeed _ _ = False

-- | True when the item has been marked "read" by the user
isRead :: ItemPredicate
isRead _ (Just i) | itemRead i = True
isRead _ _ = False

-- | Logical conjunction of @ItemPredicate@s (true when both predicates are true)
andP :: ItemPredicate -> ItemPredicate -> ItemPredicate
f `andP` g = \i is -> f i is && g i is

-- | Logical disjunction of @ItemPredicate@s (true when either predicate is true)
orP :: ItemPredicate -> ItemPredicate -> ItemPredicate
f `orP` g = \i is -> f i is || g i is

-- | Logical negation of @ItemPredicate@s (true when the predicate is false)
notP :: ItemPredicate -> ItemPredicate
notP f i is = not $ f i is
