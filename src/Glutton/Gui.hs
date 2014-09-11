module Glutton.Gui
       ( startGui ) where

import Control.Concurrent
import Control.Concurrent.Chan.Split
import Data.Maybe
import Data.Monoid
import Graphics.UI.Threepenny as UI hiding (map)
import Prelude hiding (div, span)

import Glutton.Gui.Css (stylesheet)
import Glutton.Subscription as S
import Glutton.Subscription.Updater

startGui :: Int -> Updater -> IO ()
startGui port u = do
  (ucE, trigger) <- newEvent
  rPort <- getPort u
  initialSubs <- getFeeds u
  ucB <- stepper [] ucE
  trigger =<< mapM unreadCount initialSubs
  t <- forkIO $ listenThread trigger rPort
    
  UI.startGUI defaultConfig { tpPort = Just port, tpStatic = Nothing } $ setup ucB u
  killThread t

type UnreadCount = (String,Int,SubscriptionHandle)
  
unreadCount :: SubscriptionHandle -> IO UnreadCount
unreadCount h = do
  s <- S.get h
  return (feedTitle s, length $ filter (not . itemRead) $ feedItems s, h)

listenThread :: Handler [UnreadCount] -> ReceivePort [SubscriptionHandle] -> IO ()
listenThread trigger port = do
  handles <- receive port
  trigger =<<  mapM unreadCount handles
  listenThread trigger port

setup :: Behavior [UnreadCount] -> Updater -> Window -> UI ()
setup ucs updater window = do
  activeFeed <- div # set id_ "activeFeed"

  let sidebarContents = fmap (mapM $ mkSidebar activeFeed) ucs
  contents <- currentValue sidebarContents
  contents' <- contents
  sidebar <- div # set id_ "sidebar" #+ map element contents'
  onChanges sidebarContents $ \c -> do
    c' <- c
    element sidebar # set children c'

  _ <- getHead window #+ [mkElement "style" # set text stylesheet]
  _ <- getBody window #+ map element [sidebar, activeFeed]
  return ()

mkSidebar :: Element -> UnreadCount -> UI Element
mkSidebar activeFeed (n, c, h) = do
  let countString = if c > 0 then show c else ""
  feedSelector <- div #+ [ span # set text n
                         , span # set text countString #. "unreadCount" ]
  on click feedSelector $ const $ mkActiveFeed activeFeed h
  return feedSelector

mkActiveFeed :: Element -> SubscriptionHandle -> UI ()
mkActiveFeed activeFeed handle = do
  sub <- liftIO $ S.get handle
  markAllReadButton <- span #. "button" # set text "Mark All Read"
  header <- div #. "activeFeedTitle" #+
            [ span # set text (feedTitle sub)
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
  markRead <- span # set text (check $ itemRead i)
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
  div #+ [ element markRead
         , element link ]
  where check True  = "☑"
        check False = "☐"
