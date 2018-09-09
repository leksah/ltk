{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.Basics
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for the basiscs of composing GUIs from editors
--
-----------------------------------------------------------------------------------

module Graphics.UI.Editor.Basics (
-- * Types
    Getter
,   Setter
,   Injector
,   Extractor
,   Applicator
,   Editor
,   emptyNotifier
,   GUIEvent(..)
,   GUIEventSelector(..)
,   GtkRegFunc
,   Notifier(..)
,   GtkHandler
,   Connection(..)
,   Connections

,   activateEvent
,   propagateEvent
,   allGUIEvents
,   genericGUIEvents
,   propagateAsChanged
) where

import Prelude ()
import Prelude.Compat

import Data.Unique
import Data.IORef
import Data.Text (Text)
import Control.Monad (foldM, void)
import Control.Monad.Trans (liftIO)

import Graphics.UI.Editor.Parameters
import Control.Event
import Data.Map (Map)
import qualified Data.Map as Map  (delete,insert,lookup,empty)
import Data.Maybe (isJust, fromJust)
import Control.Arrow (first)
import GI.Gtk.Objects.Widget
       (afterWidgetKeyReleaseEvent, afterWidgetButtonReleaseEvent,
        onWidgetFocusInEvent, onWidgetFocusOutEvent, widgetGetName,
        Widget(..))
import Data.GI.Base.BasicTypes (GObject)
import Data.GI.Base.Signals (SignalHandlerId)
import Data.GI.Base.ManagedPtr (unsafeCastTo, castTo)
import GI.Gtk.Objects.Button (onButtonClicked, Button(..))
import Control.Monad.IO.Class (MonadIO)

-- ---------------------------------------------------------------------
-- * Basic Types
--

--
-- | A type for getting a field of a record
--
type Getter alpha beta     =   alpha -> beta
--
-- | A type for setting the field of a record
--
type Setter alpha beta     =   beta -> alpha -> alpha

--
-- | A type for injecting a value into an editor
--
type Injector beta     =   beta -> IO ()
--
-- | A type for extracting a value from an editor
--
type Extractor beta    =   IO (Maybe beta)
--
-- | A type for the application of a value to be reflected in the GUI
--
type Applicator beta gamma  =   beta -> gamma ()

--
-- | A type to describe an editor.
-- alpha is the type of the individual field of the record
type Editor alpha  =   Parameters -> Notifier
                        -> IO(Widget, Injector alpha , Extractor alpha)


--
-- | A type for an event in the GUI
--
data GUIEvent = GUIEvent {
    selector :: GUIEventSelector
,   eventText :: Text
,   gtkReturn :: Bool -- ^ True means that the event has been completely handled,
                      --  gtk shoudn't do any further action about it (Often not
                      --  a good idea
}
instance Event GUIEvent GUIEventSelector where
    getSelector = selector

data GUIEventSelector = FocusOut        -- ^ generic, the widget looses the focus
                    |   FocusIn         -- ^ generic, the widget gets the focus
                    |   ButtonPressed   -- ^ generic, a mouse key has been pressed and released, while the widget has the focus
                    |   KeyPressed      -- ^ generic, a keyboard key has been pressed and released, while the widget has the focus
                    |   Clicked         -- ^ button specific, the button has been pressed
                    |   MayHaveChanged  -- ^ generic, no gui event, the contents of the widget may have changed
                    |   ValidationError -- ^ validation of a contents has failed
    deriving (Eq,Ord,Show,Enum,Bounded)

instance EventSelector GUIEventSelector

allGUIEvents :: [GUIEventSelector]
allGUIEvents = [minBound .. maxBound]
genericGUIEvents :: [GUIEventSelector]
genericGUIEvents = [FocusOut,FocusIn,ButtonPressed,KeyPressed]

-- ------------------------------------------------------------
-- * Implementation of GUI event system
-- ------------------------------------------------------------

--  | A type for handling an IO event
--  Returning True: The event has been handles
--  Returning False: Handling should proceed
type GtkHandler = IO Bool

--
-- | A type for a function to register a gtk event
-- |
type GtkRegFunc = forall o . GObject o => o -> GtkHandler -> IO Connection

--
-- | The widgets are the real event sources.
-- The GtkRegFunc is the function used to register the event.
-- The connectIds are set, when the event is activated, and
-- can be used to deactivate the event.
-- Or it is a propagated event and:
-- The Notifier List is a list of event sources, to which registrations
-- are propageted.
-- The last map is used to unregister propagated events properly
--
type GUIEventReg =  ([Connection],
                        ([Notifier], Map Unique [(Unique,Notifier)]))

--
-- | The event source in the gtk editor context
-- If the second argument is Left Handler the handler gets registered
-- If the second argument is Right Unique the handler will be removed
-- The returned unique value must be used for unregistering an event
newtype Notifier =   Noti (IORef (Handlers GUIEvent IO GUIEventSelector,
                                        Map GUIEventSelector GUIEventReg))


emptyNotifier :: MonadIO m => m Notifier
emptyNotifier                            =   do
    h <- liftIO $ newIORef (Map.empty,Map.empty)
    let noti      =  Noti h
    return noti

--
-- | Signal handlers for the different pane types
--
data Connection =  forall alpha . GObject alpha => ConnectC alpha SignalHandlerId

type Connections = [Connection]

instance  EventSource Notifier GUIEvent IO GUIEventSelector where
    getHandlers (Noti pairRef)              =   do
        (h,_) <- readIORef pairRef
        return h

    setHandlers (Noti pairRef) h            =   do
        (_,r) <- readIORef pairRef
        writeIORef pairRef (h,r)

    myUnique _                              =   newUnique

    canTriggerEvent _ _                     =   True

    registerEvent o@(Noti pairRef) eventSel hand =   do
        (handlers, ger)     <-  readIORef pairRef
        unique              <-  myUnique o
        newGer <- case Map.lookup eventSel ger of
                    Nothing -> return ger
                    Just (_,([],_um))  -> return ger
                    Just (cids,(notifiers,um))  -> do
                        lu <-  mapM (\es -> registerEvent es eventSel hand)
                                        notifiers
                        let jl =  map (first fromJust)
                                        $ filter (isJust.fst)
                                            $ zip lu notifiers
                        let newUm = Map.insert unique jl um
                        return (Map.insert eventSel (cids,(notifiers,newUm)) ger)
        let newHandlers =   case eventSel `Map.lookup` handlers of
                                Nothing -> Map.insert eventSel
                                                [(unique,hand)] handlers
                                Just l  -> Map.insert eventSel
                                                ((unique,hand):l) handlers
        writeIORef pairRef (newHandlers,newGer)
        return (Just unique)

    unregisterEvent (Noti pairRef) eventSel unique =   do
        (handlers, ger) <- readIORef pairRef
        newGer <- case Map.lookup eventSel ger of
            Nothing -> return ger
            Just (cids,(notis,um))  ->
                case unique `Map.lookup` um of
                    Nothing ->  return ger
                    Just l  ->  do
                        mapM_  (\(u,es) -> unregisterEvent es eventSel u) l
                        let newUm = unique `Map.delete` um
                        return (Map.insert eventSel (cids,(notis,newUm)) ger)
        let newHandlers =   case eventSel `Map.lookup` handlers of
                                Nothing -> handlers
                                Just l -> case filter (\ (mu,_) -> mu /= unique) l of
                                            [] -> Map.delete eventSel handlers
                                            l'  -> Map.insert eventSel l' handlers
        writeIORef pairRef (newHandlers,newGer)
        return ()

--
-- | Propagate the event with the selector from notifier to eventSource
--
propagateEvent :: MonadIO m => Notifier -> [Notifier] -> GUIEventSelector -> m ()
propagateEvent (Noti pairRef) eventSources eventSel = do
    (handlers,ger) <- liftIO $ readIORef pairRef
    let newGer =    case Map.lookup eventSel ger of
                            Nothing -> Map.insert eventSel
                                        ([],(eventSources,Map.empty)) ger
                            Just (w,(notiList,unregMap)) -> Map.insert eventSel
                                (w,(eventSources ++ notiList,unregMap)) ger
    --now propagate already registered events
    newGer2 <- case eventSel `Map.lookup` handlers of
        Nothing     ->  return newGer
        Just hl     ->  foldM (repropagate eventSel) newGer hl
    liftIO $ writeIORef pairRef (handlers,newGer)
  where
    repropagate :: MonadIO m
        =>  GUIEventSelector
        ->  Map GUIEventSelector GUIEventReg
        ->  (Unique, GUIEvent -> IO GUIEvent)
        ->  m (Map GUIEventSelector GUIEventReg)
    repropagate _eventSet ger (unique,hand) =
        case Map.lookup eventSel ger of
            Just (cids,(notifiers,um))
                -> do
                    lu <-  mapM (\es -> liftIO $ registerEvent es eventSel hand)
                                    notifiers
                    let jl =  map (first fromJust)
                                    $ filter (isJust.fst)
                                        $ zip lu notifiers
                    let newUm = Map.insert unique jl um
                    return (Map.insert eventSel (cids,(notifiers,newUm)) ger)
            _   ->  error "Basics>>propagateEvent: impossible case"

--
-- | Activate the event after the event has been declared and the
-- widget has been constructed
--

activateEvent
  :: GObject o =>
     o
     -> Notifier
     -> Maybe (o -> IO Bool -> IO SignalHandlerId)
     -> GUIEventSelector
     -> IO ()
activateEvent widget (Noti pairRef) mbRegisterFunc eventSel = do
    let registerFunc = maybe (getStandardRegFunction eventSel)
                            (\f w h -> ConnectC w <$> f w h) mbRegisterFunc
    cid <- registerFunc widget (do
                (hi,_) <- readIORef pairRef
                case Map.lookup eventSel hi of
                    Nothing -> return False
                    Just [] -> return False
                    Just handlers -> do
                        name <- castTo Widget widget >>= \case
                                    Just w  -> widgetGetName w
                                    Nothing -> return "no widget - no name"
                        eventList <- mapM ((\f -> do
                            let ev = GUIEvent eventSel "" False
                            f ev) . snd) handlers
                        let boolList = map gtkReturn eventList
                        return (and boolList))
    (handerls,ger) <- readIORef pairRef
    let newGer      =   case Map.lookup eventSel ger of
                            Nothing ->  Map.insert eventSel ([cid],([],Map.empty))
                                        ger
                            Just (cids,prop) ->
                                        Map.insert eventSel (cid:cids,prop) ger
    writeIORef pairRef (handerls,newGer)

--
-- | A convinence method for not repeating this over and over again
--
getStandardRegFunction :: GUIEventSelector -> GtkRegFunc
getStandardRegFunction FocusOut         =   \w h -> fmap (ConnectC w) $ unsafeCastTo Widget w >>= (`onWidgetFocusOutEvent` const h)
getStandardRegFunction FocusIn          =   \w h -> fmap (ConnectC w) $ unsafeCastTo Widget w >>= (`onWidgetFocusInEvent` const h)
getStandardRegFunction ButtonPressed    =   \w h -> fmap (ConnectC w) $ unsafeCastTo Widget w >>= (`afterWidgetButtonReleaseEvent` const h)
getStandardRegFunction KeyPressed       =   \w h -> fmap (ConnectC w) $ unsafeCastTo Widget w >>= (`afterWidgetKeyReleaseEvent` const h)
getStandardRegFunction Clicked          =   \w h -> fmap (ConnectC w) $ unsafeCastTo Button w >>= (`onButtonClicked` void h)
getStandardRegFunction _    =   error "Basic>>getStandardRegFunction: no original GUI event"

propagateAsChanged
  :: (EventSource alpha GUIEvent m GUIEventSelector) =>
     alpha -> [GUIEventSelector] -> m ()
propagateAsChanged notifier =
    mapM_ (\s -> registerEvent notifier s
            (\ e -> triggerEvent notifier e{selector = MayHaveChanged}))
