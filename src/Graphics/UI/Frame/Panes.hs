{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.Panes
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional21
-- Portability :  portable
--
-- | The basic definitions for all panes
--
-------------------------------------------------------------------------------

module Graphics.UI.Frame.Panes (

-- * Panes and pane layout
    PaneMonad(..)
,   IDEPane(..)
,   Pane(..)
,   RecoverablePane(..)
,   PaneDirection(..)
,   PanePathElement(..)
,   PanePath
,   PaneLayout(..)
,   PaneName
,   Connection(..)
,   Connections
,   StandardPath
,   FrameState(..)
,   signalDisconnectAll
) where

import Prelude ()
import Prelude.Compat
import Control.Applicative (Applicative)
import Data.Map (Map)
import Data.Typeable
import Graphics.UI.Editor.Basics
       (Connection(..), Connection, Connections)
import Control.Monad.IO.Class (MonadIO(..), MonadIO)
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.Text as T (pack)
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import GI.Gtk.Objects.Widget (Widget(..))
import GI.Gtk.Objects.Notebook (Notebook(..))
import GI.Gtk.Objects.Window (Window(..))
import GI.Gtk.Objects.UIManager (UIManager(..))
import GI.GObject.Functions (signalHandlerDisconnect)
import GI.GObject.Objects.Object (Object(..))
import Foreign.Ptr (Ptr)

-- ---------------------------------------------------------------------
-- Panes and pane layout
--

--
-- | A path to a pane
--
type PanePath       =   [PanePathElement]

--
-- | An element of a path to a pane
--
data PanePathElement = SplitP PaneDirection | GroupP Text
    deriving (Eq, Show, Read, Generic)

instance ToJSON PanePathElement
instance FromJSON PanePathElement

--
-- | The relative direction to a pane from the parent
--
data PaneDirection  =   TopP | BottomP | LeftP | RightP
    deriving (Eq, Show, Read, Generic)

instance ToJSON PaneDirection
instance FromJSON PaneDirection

--
-- | Description of a window layout
-- Horizontal: top bottom Vertical: left right
--
data PaneLayout =       HorizontalP PaneLayout PaneLayout Int
                    |   VerticalP PaneLayout PaneLayout Int
                    |   TerminalP {
                                paneGroups   :: Map Text PaneLayout
                            ,   paneTabs     :: Maybe PaneDirection
                            ,   currentPage  :: Int
                            ,   detachedId   :: Maybe Text
                            ,   detachedSize :: Maybe (Int, Int) }
    deriving (Eq, Show, Read, Generic)

instance ToJSON PaneLayout
instance FromJSON PaneLayout

--
-- | All kinds of panes are instances of pane
--

class (Typeable alpha, PaneMonad delta) =>  Pane alpha delta | alpha -> delta  where

    getTopWidget    ::   alpha -> delta Widget
    -- ^ gets the top Widget of this pane
    paneId          ::   alpha -> Text
    primPaneName    ::   alpha -> Text

    paneName        ::   alpha -> PaneName
    paneName b      =   if getAddedIndex b == 0
                            then primPaneName b
                            else primPaneName b <> " (" <> T.pack (show $ getAddedIndex b) <> ")"

    paneTooltipText :: alpha -> Maybe Text
    paneTooltipText _p = Nothing

    getAddedIndex   ::   alpha -> Int
    getAddedIndex _ =   0


class (Pane alpha delta, Typeable beta, Show beta, Read beta) => RecoverablePane alpha beta delta | beta  -> alpha, alpha -> beta  where

    saveState       ::   alpha -> delta (Maybe beta)
    recoverState    ::   PanePath -> beta -> delta (Maybe alpha)

    builder         ::   PanePath -> Notebook -> Window -> delta (Maybe alpha,Connections)

    -- getEditor    ::   Editor alpha

    makeActive      ::   alpha -> delta ()
    makeActive pane =   activateThisPane pane []

    closePane       ::   alpha -> delta Bool
    closePane       =   closeThisPane

    getPane         ::  delta (Maybe alpha)
    getPane         =   getThisPane

    forceGetPane    ::  Either PanePath Text  -> delta alpha
    forceGetPane pp =   do  mbPane <- getOrBuildPane pp
                            case mbPane of
                                Nothing -> error "Can't get pane "
                                Just p -> return p

    getOrBuildPane  ::  Either PanePath Text -> delta (Maybe alpha)
    getOrBuildPane  =   getOrBuildThisPane

    displayPane     ::  alpha -> Bool -> delta ()
    displayPane     =   displayThisPane

    getAndDisplayPane :: Either PanePath Text -> Bool  -> delta (Maybe alpha)
    getAndDisplayPane pps b = do
        mbP <- getOrBuildThisPane pps
        case mbP of
            Nothing -> return Nothing
            Just p  -> do
                displayPane p b
                return (Just p)

    buildPane       ::  PanePath ->
                        Notebook ->
                        (PanePath -> Notebook -> Window -> delta (Maybe alpha,Connections)) ->
                        delta (Maybe alpha)
    buildPane       =   buildThisPane



class (Applicative delta, MonadIO delta) =>  PaneMonad delta where
    setFrameState   ::  FrameState delta -> delta ()
    getFrameState   ::  delta (FrameState delta)
    runInIO         ::  forall alpha beta. (beta -> delta alpha) -> delta (beta -> IO alpha)
    panePathForGroup::  Text -> delta PanePath

    getThisPane     ::  forall alpha beta . RecoverablePane alpha beta delta => delta (Maybe alpha)
    displayThisPane ::  forall alpha beta . RecoverablePane alpha beta delta => alpha -> Bool -> delta ()
    getOrBuildThisPane
                    ::  forall alpha beta . RecoverablePane alpha beta delta => Either PanePath Text -> delta (Maybe alpha)
    buildThisPane   ::  forall alpha beta . RecoverablePane alpha beta delta =>
                        PanePath ->
                        Notebook ->
                        (PanePath -> Notebook -> Window -> delta (Maybe alpha,Connections)) ->
                        delta (Maybe alpha)
    activateThisPane :: forall alpha beta . RecoverablePane alpha beta delta =>  alpha -> Connections -> delta ()
    closeThisPane   ::  forall alpha beta . RecoverablePane alpha beta delta =>  alpha -> delta Bool

type PaneName = Text

data IDEPane delta       =   forall alpha beta. (RecoverablePane alpha beta delta) => PaneC alpha

instance Eq (IDEPane delta) where
    (==) (PaneC x) (PaneC y) = paneName x == paneName y

instance Ord (IDEPane delta) where
    (<=) (PaneC x) (PaneC y) = paneName x <=  paneName y

instance Show (IDEPane delta) where
    show (PaneC x)    = show $ "Pane " <> paneName x

type StandardPath = PanePath

data FrameState delta = FrameState {
    windows         ::  [Window]
,   uiManager       ::  UIManager
,   panes           ::  Map PaneName (IDEPane delta)
,   paneMap         ::  Map PaneName (PanePath, Connections)
,   activePane      ::  (Maybe (PaneName, Connections), [PaneName])
,   panePathFromNB  ::  ! (Map (Ptr Notebook) PanePath)
,   layout          ::  PaneLayout}
    deriving Show

instance Show Window where
    show _ = "a Window"

instance Show UIManager where
    show _ = "a UIManager"

instance Show Connection where
    show _ = "a Connection"

instance Show Notebook where
    show _ = "a Notebook"

signalDisconnectAll :: MonadIO m => Connections -> m ()
signalDisconnectAll = mapM_ (\ (ConnectC o s) -> liftIO (unsafeCastTo Object o) >>= flip signalHandlerDisconnect s)

