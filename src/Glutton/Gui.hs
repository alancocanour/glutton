{- |
Module      : Glutton.Gui
Description : threepeeny-gui graphical user interface
-}
module Glutton.Gui
       ( createGui ) where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Graphics.UI.Threepenny as UI hiding (map, p, sub, header, link)
import Prelude hiding (div, span)

import Glutton.Gui.Css (stylesheet)
import Glutton.Subscription as S
import Glutton.Subscription.Updater

-- | Initializes the Threepenny Graphical User Interface and returns
-- functions to start and update it
createGui
  :: IO ( Int -> Updater -> IO ()
        , [SubscriptionHandle] -> IO ()) -- ^ A function to start the GUI given the port to run on and an 'Updater' and a function to update the GUI when the subscriptions change
createGui = do
  (unreadCountE, trigger) <- newEvent
  unreadCountB <- stepper [] unreadCountE
  return (\p u -> UI.startGUI defaultConfig { jsPort = Just p, jsStatic = Nothing } $ setup unreadCountB u
         ,trigger <=< mapM unreadCount)

-- | A feed title, number of unread items, and 'SubscriptionHandle'
type UnreadCount = (String,Int,SubscriptionHandle)

-- | Convert a 'SubscriptionHandle' into an 'UnreadCount'
unreadCount :: SubscriptionHandle -> IO UnreadCount
unreadCount h = do
  s <- S.get h
  return (feedTitle s, length $ filter (not . itemRead) $ feedItems s, h)

-- | Get the count of all unread items
sumUnreadCount :: [UnreadCount] -> Int
sumUnreadCount = sum . map (\(_,c,_)->c)

-- | Constructs the GUI
setup :: Behavior [UnreadCount] -> Updater -> Window -> UI ()
setup ucs updater window = do --TODO use the Updater to enable adding/removing subscriptions from the GUI
  _ <- sink title (fmap ((++ " - Glutton") . show . sumUnreadCount) ucs) (return window)

  activeFeed <- div # set id_ "activeFeed"

  let sidebarContents = fmap (map $ mkSidebar activeFeed) ucs
  sidebar <- div
             # set id_ "sidebar"
  sinkChildren sidebarContents (element sidebar)

  _ <- getHead window #+ [mkElement "style" # set text stylesheet]
  _ <- getBody window #+ map element [sidebar, activeFeed]
  return ()

-- | Like Threepenny's 'sink' function but lets you use a Behavior [UI Element]
sinkChildren :: Behavior [UI Element] -> UI Element -> UI ()
sinkChildren b e = do
  _ <- (e #+) =<< currentValue b
  onChanges b $ \elements -> do
    elements' <- sequence elements
    e # set children elements'

mkSidebar :: Element -> UnreadCount -> UI Element
mkSidebar activeFeed (n, c, h) = do
  let countString = if c > 0 then show c else ""
  feedSelector <- div #. "feedSelector"
                  #+ [ span # set text n
                     , span # set text countString #. "unreadCount" ]
  on click feedSelector $ const $ mkActiveFeed activeFeed h
  return feedSelector

mkActiveFeed :: Element -> SubscriptionHandle -> UI ()
mkActiveFeed activeFeed handle = do
  sub <- liftIO $ S.get handle
  markAllReadButton <- span #. "button" # set text "♺ Mark All Read"
  header <- div #+
            [ span #. "activeFeedTitle" # set text (feedTitle sub)
            , element markAllReadButton ]
  _ <- element activeFeed # set children []
  _ <- element activeFeed #+ (element header
                              : map (mkItem handle) (feedItems sub) )

  on click markAllReadButton $ const $ do
    liftIO $ modify handle $ \s ->
      let items' = map (\i -> i {itemRead = True}) $ feedItems s
      in s { feedItems = items'}
    mkActiveFeed activeFeed handle

mkItem :: SubscriptionHandle -> ItemState -> UI Element
mkItem sh i = do
  markRead <- span #. "clickable" # set text (check $ itemRead i)
  on click markRead $ const $ do
    newReadState <- liftIO $ modifyAndRead sh $ \s ->
      let items' = flip map (feedItems s) $ \item ->
            if itemId item == itemId i then
              let newReadState = not $ itemRead item
              in (item { itemRead = newReadState }, First $ Just newReadState)
            else (item, First Nothing)
      in (s { feedItems = map fst items' }, getFirst $ mconcat $ map snd items')
    element markRead # set text (check $ fromMaybe True newReadState)
  
  link <- a # set (attr "target") "_blank"
            # set (attr "href") (fromMaybe "" $ itemLink i)
            # set text (fromMaybe "" $ itemTitle i)
            #. if itemRead i then "read" else "unread"
  div #. "feedItem"
    #+ [ element markRead
       , element link ]
  where check True  = "☑"
        check False = "☐"
