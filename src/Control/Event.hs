{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
--
-- Module      :  Control.Event
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | A simple event mechanism
--
-------------------------------------------------------------------------------
module Control.Event (
    EventSelector
,   Event(..)
,   EventSource(..)
,   Handlers

,   registerEvents
) where

import Prelude ()
import Prelude.Compat
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Unique
import Control.Monad
import Data.Maybe (catMaybes)

-- | Every event needs a selector, which should identify the type of event
class (Eq delta, Ord delta, Show delta) =>  EventSelector delta

-- | Every event needs to know its selector and its source
class EventSelector delta => Event beta delta | beta -> delta, delta -> beta where
    getSelector ::  beta -> delta

-- | This shows the implementation of the event mechnism
type Handlers beta gamma delta = Map delta [(Unique, beta  -> gamma beta)]

-- | Everything which is an event source needs this
-- alpha is the Notifier
-- beta is the event
-- gamma is the monad
-- delta is the event selector
class (Monad gamma, Event beta delta) => EventSource alpha beta gamma delta
        | alpha -> beta, alpha -> gamma where
    getHandlers     ::  alpha -> gamma (Handlers beta gamma delta)
    setHandlers     ::  alpha -> Handlers beta gamma delta -> gamma ()
    myUnique        ::  alpha -> gamma Unique

    -- | Reimplement this in instances to make triggering of events possible
    canTriggerEvent :: alpha -> delta -> Bool
    canTriggerEvent _ _             =   False

    -- | Returns the event, so that you may get values back from an event
    -- Args: Notifier, Event
    triggerEvent ::  alpha -> beta -> gamma beta
    triggerEvent o e    =
        if canTriggerEvent o (getSelector e)
            then do
                handlerMap  <-  getHandlers o
                let selector    =   getSelector e
                case selector `Map.lookup` handlerMap of
                    Nothing     ->  return e
                    Just l      ->  foldM (\e' (_,ah) -> ah e') e (reverse l)
            else error $ "Can't trigger event " ++ show (getSelector e)

    -- returns Unique if registration was successfull, else Nothing
    -- Args: Notifier, EventSelector, Handler (Event -> Monad Event)
    registerEvent   ::  alpha -> delta -> (beta -> gamma beta) -> gamma (Maybe Unique)
    registerEvent o e handler =
        if canTriggerEvent o e
            then do
                handlerMap  <-  getHandlers o
                unique      <-  myUnique o
                let newHandlers =   case e `Map.lookup` handlerMap of
                                        Nothing -> Map.insert e [(unique,handler)] handlerMap
                                        Just l  -> Map.insert e ((unique,handler):l) handlerMap
                setHandlers o newHandlers
                return (Just unique)
            else error $ "Can't register event " ++ show e

    -- | use Left to register and Right to unregister
    -- Args: Notifier, EventSelector, Unique
    unregisterEvent   ::  alpha -> delta -> Unique -> gamma ()
    unregisterEvent o e unique =
        if canTriggerEvent o e
            then do
                handlerMap  <-  getHandlers o
                let newHandlers =   case e `Map.lookup` handlerMap of
                                        Nothing -> handlerMap
                                        Just l -> let newList = filter (\ (mu,_) -> mu /= unique) l
                                                  in  Map.insert e newList handlerMap
                setHandlers o newHandlers
                return ()
            else error $ "Can't register event " ++ show e

registerEvents :: EventSource alpha beta gamma delta => alpha -> [delta] -> (beta -> gamma beta) -> gamma [Unique]
registerEvents o l handler = catMaybes <$> mapM (\ e -> registerEvent o e handler) l

