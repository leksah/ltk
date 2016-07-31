{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.ViewFrame
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Splittable panes containing notebooks with any widgets
--
---------------------------------------------------------------------------------


module Graphics.UI.Frame.ViewFrame (
    removePaneAdmin
,   addPaneAdmin
,   notebookInsertOrdered
,   markLabel

-- * Convenience methods for accesing Pane state
,   posTypeToPaneDirection
,   paneDirectionToPosType
,   paneFromName
,   mbPaneFromName
,   guiPropertiesFromName

-- * View Actions
,   viewMove
,   viewMoveTo
,   viewSplitHorizontal
,   viewSplitVertical
--,   viewSplit
,   viewSplit'
,   viewNewGroup
,   newGroupOrBringToFront
,   bringGroupToFront
,   viewNest
,   viewNest'
,   viewDetach
,   viewDetach'
,   handleNotebookSwitch
,   viewCollapse
,   viewCollapse'
,   viewTabsPos
,   viewSwitchTabs

,   closeGroup
,   allGroupNames

-- * View Queries
,   getBestPanePath
,   getBestPathForId
,   getActivePanePath
,   getActivePanePathOrStandard
,   figureOutPaneName
,   getNotebook
,   getPaned
,   getActiveNotebook
,   getActivePane
,   setActivePane
,   getUiManager
,   getWindows
,   getMainWindow
,   getActiveWindow
,   getActiveScreen
,   getLayout
,   getPanesSt
,   getPaneMapSt
,   getPanePrim
,   getPanes

-- * View Actions
,   bringPaneToFront
,   newNotebook
,   newNotebook'

-- * Accessing GUI elements
--,   widgetFromPath
,   getUIAction
,   widgetGet

,   initGtkRc
) where

import Prelude ()
import Prelude.Compat
import Control.Applicative (Applicative)
import qualified Data.Map as Map
import Data.List (findIndex, isPrefixOf, deleteBy, stripPrefix, elemIndex)
import Data.Maybe
import Data.Unique
import Data.Typeable
import Data.Text (Text)

import Graphics.UI.Frame.Panes
import Graphics.UI.Editor.Parameters
import System.CPUTime (getCPUTime)
import MyMissing (forceJust, forceHead)
import Graphics.UI.Editor.MakeEditor
    (mkField, FieldDescription(..), buildEditor)
import Graphics.UI.Editor.Simple (stringEditor, textEditor, okCancelFields)
import Control.Event (registerEvent)
import Graphics.UI.Editor.Basics
    (eventText, GUIEventSelector(..))
import qualified Data.Set as  Set (unions, member)
import Data.Set (Set(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when, liftM, foldM)
import Control.Applicative ((<$>))
import qualified Control.Monad.Reader as Gtk (liftIO)
import qualified Data.Text as T (pack, stripPrefix, unpack)
import Data.Monoid ((<>))
import Data.Foldable (forM_)
import Control.Arrow (Arrow(..))
import GI.Gtk
       (noWidget, windowGetScreen, Bin(..), CssProvider(..),
        uIManagerGetAction, Action, toWidget, selectionDataGetText,
        SelectionData, WidgetDragDataReceivedCallback,
        onWidgetDragDataReceived, widgetDragDestSetTargetList,
        widgetDragDestSet, setNotebookEnablePopup, notebookSetScrollable,
        widgetSetSizeRequest, notebookNew, windowPresent, getWidgetVisible,
        notebookRemovePage, Window(..), widgetGetToplevel,
        onWidgetDeleteEvent, widgetGetAllocation, windowSetDefaultSize,
        setWidgetName, windowNew, Widget(..), dialogResponse, HBox(..),
        dialogGetActionArea, VBox(..), dialogGetContentArea,
        setWindowTitle, setWindowTransientFor, dialogNew, Window,
        windowSetTransientFor, setMessageDialogText,
        constructDialogUseHeaderBar, MessageDialog(..), widgetDestroy,
        dialogRun, afterNotebookSwitchPage, widgetGrabFocus,
        boxReorderChild, Box(..), notebookSetMenuLabel,
        notebookSetTabLabel, notebookInsertPage, Paned(..), panedPack1,
        containerRemove, notebookPageNum, Notebook(..), panedPack2,
        hPanedNew, toPaned, Container(..), vPanedNew, widgetGetParent,
        notebookGetCurrentPage, notebookSetTabPos, PositionType(..),
        notebookSetShowTabs, notebookGetShowTabs, Label(..),
        containerGetChildren, binGetChild, notebookGetTabLabel,
        labelSetMarkup, labelSetUseMarkup, onWidgetButtonReleaseEvent,
        onButtonClicked, selectionDataSetText, onWidgetDragDataGet,
        widgetDragSourceSetTargetList, targetListAddTextTargets,
        targetListNew, widgetDragSourceSet, setWidgetHalign,
        setWidgetValign, cssProviderLoadFromData, boxPackStart,
        containerAdd, containerSetBorderWidth, styleContextAddProvider,
        widgetGetStyleContext, cssProviderNew, pattern STOCK_CLOSE,
        imageNewFromStock, imageNewFromPixbuf, iconThemeLoadIcon,
        iconThemeGetDefault, buttonSetAlignment, buttonSetRelief,
        buttonSetFocusOnClick, buttonNew, hBoxNew,
        eventBoxSetVisibleWindow, eventBoxNew, miscSetPadding,
        miscSetAlignment, EventBox, notebookSetCurrentPage, widgetShowAll,
        notebookInsertPageMenu, widgetGetName, notebookGetNthPage,
        notebookGetNPages, labelNew, Label, IsWidget, IsNotebook,
        widgetSetName)
import GI.Gtk.Enums
       (WindowType(..), ResponseType(..), ButtonsType(..),
        MessageType(..), PositionType(..), Align(..), IconSize(..),
        ReliefStyle(..))
import GI.Gtk.Flags (DestDefaults(..), IconLookupFlags(..))
import GI.Gdk.Flags (ModifierType(..), DragAction(..))
import GI.Gdk
       (DragContext, Screen, getEventButtonState)
import Graphics.UI.Frame.Rectangle (getRectangleWidth, getRectangleHeight)
import Data.GI.Base
       (unsafeManagedPtrCastPtr, withManagedPtr, castTo, unsafeCastTo,
        ForeignPtrNewtype, UnexpectedNullPointerReturn(..), GObject(..),
        new', nullToNothing)
import Control.Exception (catch)
import Data.Int (Int32)
import Data.Word (Word32)
import Data.Function (on)
import Data.Coerce (coerce)
import Foreign.ForeignPtr (ForeignPtr)
import Data.GI.Gtk.ModelView.Types (equalManagedPtr)
import GI.Gtk.Objects.Dialog (constructDialogUseHeaderBar)
import GI.Gtk.Objects.MessageDialog
       (constructMessageDialogButtons, setMessageDialogMessageType)
import GI.Gtk.Objects.Label (noLabel)

-- import Debug.Trace (trace)
trace (a::String) b = b

groupPrefix = "_group_"

withoutGroupPrefix :: Text -> Text
withoutGroupPrefix s = fromMaybe s (groupPrefix `T.stripPrefix` s)


initGtkRc :: IO ()
initGtkRc = return ()

removePaneAdmin :: RecoverablePane alpha beta delta =>  alpha -> delta ()
removePaneAdmin pane = do
    panes'          <-  getPanesSt
    paneMap'        <-  getPaneMapSt
    setPanesSt      (Map.delete (paneName pane) panes')
    setPaneMapSt    (Map.delete (paneName pane) paneMap')

addPaneAdmin :: RecoverablePane alpha beta delta => alpha -> Connections -> PanePath -> delta Bool
addPaneAdmin pane conn pp = do
    panes'          <-  getPanesSt
    paneMap'        <-  getPaneMapSt
    topWidget       <-  getTopWidget pane
    widgetSetName topWidget (paneName pane)
    let b1 = case Map.lookup (paneName pane) paneMap' of
                Nothing -> True
                Just it -> False
    let b2 = case Map.lookup (paneName pane) panes' of
                Nothing -> True
                Just it -> False
    if b1 && b2
        then do
            setPaneMapSt (Map.insert (paneName pane) (pp, conn) paneMap')
            setPanesSt (Map.insert (paneName pane) (PaneC pane) panes')
            return True
        else trace
               ("ViewFrame>addPaneAdmin:pane with this name already exist" <>
                  T.unpack (paneName pane))
               $ return False

getPanePrim ::  RecoverablePane alpha beta delta => delta (Maybe alpha)
getPanePrim = do
    selectedPanes <- getPanes
    if null selectedPanes || length selectedPanes > 1
        then return Nothing
        else return (Just $ head selectedPanes)

getPanes ::  RecoverablePane alpha beta delta => delta [alpha]
getPanes = do
    panes' <- getPanesSt
    return (mapMaybe (\ (PaneC p) -> cast p) (Map.elems panes'))

notebookInsertOrdered :: PaneMonad alpha => (IsNotebook self, IsWidget child)
    => self
    -> child        -- child - the Widget to use as the contents of the page.
    -> Text
    -> Maybe Label  -- the label for the page as Text or Label
    -> Bool
    -> alpha ()
notebookInsertOrdered nb widget labelStr mbLabel isGroup = do
    label       <-  case mbLabel of
                        Nothing -> labelNew (Just labelStr)
                        Just l  -> return l
    menuLabel   <-  labelNew (Just labelStr)
    numPages    <-  notebookGetNPages nb
    mbWidgets   <-  mapM (nullToNothing . notebookGetNthPage nb) [0 .. (numPages-1)]
    let widgets =   map (`forceJust` "ViewFrame.notebookInsertOrdered: no widget") mbWidgets
    labelStrs   <-  mapM widgetGetName widgets
    let pos     =   fromMaybe (-1)
                      (findIndex
                         (\ s -> withoutGroupPrefix s > withoutGroupPrefix labelStr)
                         labelStrs)
    labelBox    <-  if isGroup then groupLabel labelStr else mkLabelBox label labelStr
    markLabel nb labelBox False
    realPos     <-  notebookInsertPageMenu nb widget (Just labelBox) (Just menuLabel) (fromIntegral pos)
    widgetShowAll labelBox
    notebookSetCurrentPage nb realPos

-- | Returns a label box
mkLabelBox :: PaneMonad alpha => Label -> Text -> alpha EventBox
mkLabelBox lbl paneName = do
    miscSetAlignment lbl 0.0 0.0
    miscSetPadding lbl 0 0

    labelBox  <- eventBoxNew
    eventBoxSetVisibleWindow labelBox False
    innerBox  <- hBoxNew False 0

    tabButton <- buttonNew
    widgetSetName tabButton "leksah-close-button"
    buttonSetFocusOnClick tabButton False
    buttonSetRelief tabButton ReliefStyleNone
    buttonSetAlignment tabButton 0.0 0.0

    iconTheme <- iconThemeGetDefault
    mbIcon <- nullToNothing $ iconThemeLoadIcon iconTheme "window-close" 10 [IconLookupFlagsUseBuiltin]
    image <- case mbIcon of
                Just i  -> imageNewFromPixbuf (Just i)
                Nothing -> imageNewFromStock STOCK_CLOSE (fromIntegral $ fromEnum IconSizeMenu)

    provider <- cssProviderNew
    cssProviderLoadFromData provider (
        ".button {\n" <>
        "-GtkButton-default-border : 0px;\n" <>
        "-GtkButton-default-outside-border : 0px;\n" <>
        "-GtkButton-inner-border: 0px;\n" <>
        "-GtkWidget-focus-line-width : 0px;\n" <>
        "-GtkWidget-focus-padding : 0px;\n" <>
        "padding: 0px;\n" <>
        "border-width: 0px;\n" <>
        "}\n" <>
        "GtkImage {\n" <>
        "padding: 0px;\n" <>
        "}\n")
    context <- widgetGetStyleContext tabButton
    styleContextAddProvider context provider 600
    context <- widgetGetStyleContext image
    styleContextAddProvider context provider 600
    setWidgetValign tabButton AlignCenter
    setWidgetValign lbl AlignCenter

    containerSetBorderWidth tabButton 0
    containerAdd tabButton image

    boxPackStart innerBox lbl       False False 0
    boxPackStart innerBox tabButton False False 0

    containerAdd labelBox innerBox
    setWidgetHalign innerBox AlignCenter
    widgetDragSourceSet labelBox [ModifierTypeButton1Mask] Nothing [DragActionCopy,DragActionMove]
    tl <- targetListNew Nothing
    targetListAddTextTargets tl 0
    widgetDragSourceSetTargetList labelBox $ Just tl
    onWidgetDragDataGet labelBox (\ cont sel id timeStamp -> do
        trace ("drag paneName=" <> T.unpack paneName) $ return ()
        selectionDataSetText sel paneName (-1)
        return ())
    cl <- runInIO closeHandler
    onButtonClicked tabButton (cl ())
    onWidgetButtonReleaseEvent labelBox $ \e -> do
        modifiers <- getEventButtonState e
        let middleButton = ModifierTypeButton2Mask
        when (middleButton `elem` modifiers) (cl ())
        return False

    return labelBox
    where
        closeHandler :: PaneMonad alpha => () -> alpha ()
        closeHandler _ =    case groupPrefix `T.stripPrefix` paneName of
                                Just group  -> closeGroup group
                                Nothing -> do
                                    (PaneC pane) <- paneFromName paneName
                                    closePane pane
                                    return ()

groupLabel :: PaneMonad beta => Text -> beta EventBox
groupLabel group = do
    label <- labelNew Nothing
    labelSetUseMarkup label True
    labelSetMarkup label ("<b>" <> group <> "</b>")
    labelBox <- mkLabelBox label (groupPrefix <> group)
    widgetShowAll labelBox
    return labelBox

-- | Add the change mark or removes it
markLabel :: (MonadIO m, IsWidget alpha, IsNotebook beta) => beta -> alpha -> Bool -> m ()
markLabel nb topWidget modified =
    nullToNothing (notebookGetTabLabel nb topWidget) >>= \case
        Nothing  -> return ()
        Just box -> liftIO (unsafeCastTo Bin box) >>= nullToNothing . binGetChild >>= \case
            Nothing -> return ()
            Just container -> do
                children <- liftIO (unsafeCastTo Container container) >>= containerGetChildren
                label <- liftIO . unsafeCastTo Label $ forceHead children "ViewFrame>>markLabel: empty children"
                text <- widgetGetName topWidget
                labelSetUseMarkup label True
                labelSetMarkup label
                    (if modified
                          then "<span foreground=\"red\">" <> text <> "</span>"
                      else text)

-- | Constructs a unique pane name, which is an index and a string
figureOutPaneName :: PaneMonad alpha => Text -> Int -> alpha (Int,Text)
figureOutPaneName bn ind = do
    bufs <- getPanesSt
    let ind = foldr (\(PaneC buf) ind ->
                if primPaneName buf == bn
                    then max ind (getAddedIndex buf + 1)
                    else ind)
                0 (Map.elems bufs)
    if ind == 0
        then return (0,bn)
        else return (ind,bn <> "(" <> T.pack (show ind) <> ")")

paneFromName :: PaneMonad alpha => PaneName -> alpha (IDEPane alpha)
paneFromName pn = do
    mbPane <- mbPaneFromName pn
    case mbPane of
        Just p -> return p
        Nothing -> error $ "ViewFrame>>paneFromName:Can't find pane from unique name " ++ T.unpack pn

mbPaneFromName :: PaneMonad alpha => PaneName -> alpha (Maybe (IDEPane alpha))
mbPaneFromName pn = do
    panes  <- getPanesSt
    return (Map.lookup pn panes)

-- |
guiPropertiesFromName :: PaneMonad alpha => PaneName -> alpha (PanePath, Connections)
guiPropertiesFromName pn = do
    paneMap <- getPaneMapSt
    case Map.lookup pn paneMap of
            Just it -> return it
            otherwise  -> error $"Cant't find guiProperties from unique name " ++ T.unpack pn

posTypeToPaneDirection PositionTypeLeft      =   LeftP
posTypeToPaneDirection PositionTypeRight     =   RightP
posTypeToPaneDirection PositionTypeTop       =   TopP
posTypeToPaneDirection PositionTypeBottom    =   BottomP

paneDirectionToPosType LeftP        =   PositionTypeLeft
paneDirectionToPosType RightP       =   PositionTypeRight
paneDirectionToPosType TopP         =   PositionTypeTop
paneDirectionToPosType BottomP      =   PositionTypeBottom

--
-- | Toggle the tabs of the current notebook
--
viewSwitchTabs :: PaneMonad alpha => alpha ()
viewSwitchTabs = do
    mbNb <- getActiveNotebook
    case mbNb of
        Nothing -> return ()
        Just nb -> do
            b <- notebookGetShowTabs nb
            notebookSetShowTabs nb (not b)

--
-- | Sets the tab position in the current notebook
--
viewTabsPos :: PaneMonad alpha => PositionType -> alpha ()
viewTabsPos pos = do
    mbNb <- getActiveNotebook
    case mbNb of
        Nothing -> return ()
        Just nb -> notebookSetTabPos nb pos

--
-- | Split the currently active pane in horizontal direction
--
viewSplitHorizontal     :: PaneMonad alpha => alpha ()
viewSplitHorizontal     = viewSplit Horizontal

--
-- | Split the currently active pane in vertical direction
--
viewSplitVertical :: PaneMonad alpha => alpha ()
viewSplitVertical = viewSplit Vertical

--
-- | The active view can be split in two (horizontal or vertical)
--
viewSplit :: PaneMonad alpha => Direction -> alpha ()
viewSplit dir = do
    mbPanePath <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> viewSplit' panePath dir

viewSplit' :: PaneMonad alpha => PanePath -> Direction -> alpha ()
viewSplit' panePath dir = do
    l <- getLayout
    case layoutFromPath panePath l of
        (TerminalP _ _ _ (Just _) _) -> trace "ViewFrame>>viewSplit': can't split detached: " return ()
        _                            -> do
            activeNotebook  <- getNotebook' "viewSplit" panePath
            ind <- notebookGetCurrentPage activeNotebook
            parent <- nullToNothing (widgetGetParent activeNotebook) >>= liftIO . unsafeCastTo Container . fromJust
            let (name,altname,paneDir,
                 oldPath,newPath) =  case dir of
                                        Horizontal  -> ("top",
                                                        "bottom",
                                                        TopP,
                                                        panePath ++ [SplitP TopP],
                                                        panePath ++ [SplitP BottomP])
                                        Vertical    -> ("left",
                                                        "right",
                                                        LeftP,
                                                        panePath ++ [SplitP LeftP],
                                                        panePath ++ [SplitP RightP])
            adjustNotebooks panePath oldPath
            frameState  <- getFrameState
            setPanePathFromNB $ Map.insert (unsafeManagedPtrCastPtr activeNotebook) oldPath (panePathFromNB frameState)
            nb  <- newNotebook newPath
            newpane <- case dir of
                          Horizontal  -> vPanedNew >>= liftIO . toPaned
                          Vertical    -> hPanedNew >>= liftIO . toPaned
            rName <- widgetGetName activeNotebook
            widgetSetName newpane rName
            widgetSetName nb altname
            panedPack2 newpane nb True False
            nbIndex <- liftIO (castTo Notebook parent) >>= \case
                            Just notebook -> notebookPageNum notebook activeNotebook
                            Nothing -> trace "ViewFrame>>viewSplit': parent not a notebook: " $ return (-1)
            containerRemove parent activeNotebook
            widgetSetName activeNotebook name
            panedPack1 newpane activeNotebook True False
            case (reverse panePath, nbIndex) of
                (SplitP dir:_, _) -> do
                    paned <- liftIO $ unsafeCastTo Paned parent
                    if dir `elem` [TopP, LeftP]
                        then panedPack1 paned newpane True False
                        else panedPack2 paned newpane True False
                (GroupP group:_, n) | n >= 0 -> do
                    parentNotebook <- liftIO $ unsafeCastTo Notebook parent
                    label <- groupLabel group
                    notebookInsertPage parentNotebook newpane (Just label) n
                    label2 <- groupMenuLabel group
                    notebookSetMenuLabel parentNotebook newpane label2
                    return ()
                ([], _) -> do
                    box <- liftIO $ unsafeCastTo Box parent
                    boxPackStart box newpane True True 0
                    boxReorderChild box newpane 2
                _ -> error "No notebook index found in viewSplit"
            widgetShowAll newpane
            widgetGrabFocus activeNotebook
            if nbIndex >= 0
                then do
                    parentNotebook <- liftIO $ unsafeCastTo Notebook parent
                    notebookSetCurrentPage parentNotebook nbIndex
                else trace "ViewFrame>>viewSplit': parent not a notebook2: " $ return ()
            handleFunc <- runInIO (handleNotebookSwitch nb)
            afterNotebookSwitchPage nb (\w i -> handleFunc $ fromIntegral i)
            adjustPanes panePath (panePath ++ [SplitP paneDir])
            adjustLayoutForSplit paneDir panePath
            nullToNothing (notebookGetNthPage activeNotebook ind) >>= \case
                Nothing -> return ()
                Just widget -> do
                    name <- widgetGetName widget
                    mbPane  <- mbPaneFromName name
                    case mbPane of
                        Just (PaneC pane) -> viewMoveTo (panePath ++ [SplitP (otherDirection paneDir)]) pane
                        Nothing -> return ()

--
-- | Two notebooks can be collapsed to one
--
viewCollapse :: PaneMonad alpha => alpha ()
viewCollapse = do
    mbPanePath        <- getActivePanePath
    forM_ mbPanePath viewCollapse'

viewCollapse' :: PaneMonad alpha => PanePath -> alpha ()
viewCollapse' panePath = trace "viewCollapse' called" $ do
    layout1           <- getLayoutSt
    case layoutFromPath panePath layout1 of
        (TerminalP _ _ _ (Just _) _) -> trace "ViewFrame>>viewCollapse': can't collapse detached: "
                                            return ()
        _                            -> do
            let newPanePath     = init panePath
            let mbOtherSidePath = otherSide panePath
            case mbOtherSidePath of
                Nothing -> trace "ViewFrame>>viewCollapse': no other side path found: " return ()
                Just otherSidePath -> do
                    nbop <- getNotebookOrPaned otherSidePath return
                    nb <- liftIO $ castTo Notebook nbop
                    case nb of
                        Nothing -> trace "ViewFrame>>viewCollapse': other side path not collapsedXX: " $
                                case layoutFromPath otherSidePath layout1 of
                                    VerticalP{}   -> do
                                        viewCollapse' (otherSidePath ++ [SplitP LeftP])
                                        viewCollapse' panePath
                                    HorizontalP{} -> do
                                        viewCollapse' (otherSidePath ++ [SplitP TopP])
                                        viewCollapse' panePath
                                    _ -> trace "ViewFrame>>viewCollapse': impossible1 " return ()
                        Just otherSideNotebook -> do
                            paneMap           <- getPaneMapSt
                            activeNotebook    <- getNotebook' "viewCollapse' 1" panePath
                            -- 1. Move panes and groups to one side (includes changes to paneMap and layout)
                            let paneNamesToMove = map (\(w,(p,_)) -> w)
                                                    $filter (\(w,(p,_)) -> otherSidePath == p)
                                                        $Map.toList paneMap
                            panesToMove       <- mapM paneFromName paneNamesToMove
                            mapM_ (\(PaneC p) -> viewMoveTo panePath p) panesToMove
                            let groupNames    =  map (\n -> groupPrefix <> n) $
                                                        getGroupsFrom otherSidePath layout1
                            mapM_ (\n -> move' (n,activeNotebook)) groupNames
                            -- 2. Remove unused notebook from admin
                            st <- getFrameState
                            let ! newMap = Map.delete (unsafeManagedPtrCastPtr otherSideNotebook) (panePathFromNB st)
                            setPanePathFromNB newMap
                            -- 3. Remove one level and reparent notebook
                            parent <- nullToNothing (widgetGetParent activeNotebook) >>= liftIO . unsafeCastTo Container . fromJust
                            grandparent <- nullToNothing (widgetGetParent parent) >>= liftIO . unsafeCastTo Container . fromJust
                            nbIndex <- liftIO $ castTo Notebook grandparent >>= \case
                                            Just notebook -> notebookPageNum notebook parent
                                            Nothing -> return (-1)
                            containerRemove grandparent parent
                            containerRemove parent activeNotebook
                            if length panePath > 1
                                then do
                                    let lasPathElem = last newPanePath
                                    case (lasPathElem, nbIndex) of
                                        (SplitP dir, _) | dir == TopP || dir == LeftP -> do
                                            paned <- liftIO $ unsafeCastTo Paned grandparent
                                            panedPack1 paned activeNotebook True False
                                        (SplitP dir, _) | dir == BottomP || dir == RightP -> do
                                            paned <- liftIO $ unsafeCastTo Paned grandparent
                                            panedPack2 paned activeNotebook True False
                                        (GroupP group, n) | n >= 0 -> do
                                            grandParentNotebook <- liftIO $ unsafeCastTo Notebook grandparent
                                            label <- groupLabel group
                                            notebookInsertPage grandParentNotebook activeNotebook (Just label) n
                                            notebookSetCurrentPage grandParentNotebook n
                                            return ()
                                        _ -> error "collapse: Unable to find page index"
                                    widgetSetName activeNotebook $panePathElementToWidgetName lasPathElem
                                else do
                                    box <- liftIO $ unsafeCastTo Box grandparent
                                    boxPackStart box activeNotebook True True 0
                                    boxReorderChild box activeNotebook 2
                                    widgetSetName activeNotebook "root"
                            -- 4. Change panePathFromNotebook
                            adjustNotebooks panePath newPanePath
                            -- 5. Change paneMap
                            adjustPanes panePath newPanePath
                            -- 6. Change layout
                            adjustLayoutForCollapse panePath

getGroupsFrom :: PanePath -> PaneLayout -> [Text]
getGroupsFrom path layout =
    case layoutFromPath path layout of
        t@(TerminalP{}) -> Map.keys (paneGroups t)
        HorizontalP{}   -> []
        VerticalP{}     -> []

viewNewGroup :: PaneMonad alpha => alpha ()
viewNewGroup = do
    mainWindow <- getMainWindow
    mbGroupName <- groupNameDialog mainWindow
    case
     mbGroupName of
        Just groupName -> do
            layout <- getLayoutSt
            if groupName `Set.member` allGroupNames layout
                then do
                    md <- new' MessageDialog [
                        constructDialogUseHeaderBar 1,
                        constructMessageDialogButtons ButtonsTypeClose]
                    setMessageDialogMessageType md MessageTypeWarning
                    setMessageDialogText md $ "Group name not unique " <> groupName
                    windowSetTransientFor md (Just mainWindow)
                    dialogRun md
                    widgetDestroy md
                    return ()
                else viewNest groupName
        Nothing -> return ()

newGroupOrBringToFront :: PaneMonad alpha => Text -> PanePath -> alpha (Maybe PanePath,Bool)
newGroupOrBringToFront groupName pp = do
    layout <- getLayoutSt
    if groupName `Set.member` allGroupNames layout
        then do
            mbPP <- bringGroupToFront groupName
            return (mbPP,False)
        else let realPath = getBestPanePath pp layout in do
            viewNest' realPath groupName
            return (Just (realPath ++ [GroupP groupName]),True)

bringGroupToFront :: PaneMonad alpha => Text -> alpha (Maybe PanePath)
bringGroupToFront groupName = do
    layout <- getLayoutSt
    case findGroupPath groupName layout   of
        Just path -> do
            widget <- getNotebookOrPaned path return
            setCurrentNotebookPages widget
            return (Just path)
        Nothing -> return Nothing


--  Yet another stupid little dialog

groupNameDialog :: (Applicative m, MonadIO m) => Window -> m (Maybe Text)
groupNameDialog parent = do
    dia                        <-   dialogNew
    setWindowTransientFor dia parent
    setWindowTitle dia "Enter group name"
    upper                      <-   dialogGetContentArea dia >>= liftIO . unsafeCastTo VBox
    lower                      <-   dialogGetActionArea dia >>= liftIO . unsafeCastTo HBox
    (widget,inj,ext,_)         <-   buildEditor moduleFields ""
    (widget2,_,_,notifier)     <-   buildEditor okCancelFields ()
    liftIO $ registerEvent notifier ButtonPressed (\e -> do
            case eventText e of
                "Ok"    ->  dialogResponse dia (fromIntegral $ fromEnum ResponseTypeOk)
                _       ->  dialogResponse dia (fromIntegral $ fromEnum ResponseTypeCancel)
            return e)
    boxPackStart upper widget True True 7
    boxPackStart lower widget2 False False 7
    widgetShowAll dia
    resp  <- dialogRun dia
    value <- liftIO $ ext ""
    widgetDestroy dia
    case toEnum $ fromIntegral resp of
        ResponseTypeOk | value /= Just "" -> return value
        _                                 -> return Nothing
  where
        moduleFields :: FieldDescription Text
        moduleFields = VFD emptyParams [
                mkField
                    (paraName <<<- ParaName "New group "
                            $ emptyParams)
                    id
                    const
            (textEditor (const True) True)]

viewNest :: PaneMonad alpha => Text -> alpha ()
viewNest group = do
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> viewNest' panePath group

viewNest' :: PaneMonad alpha => PanePath -> Text -> alpha ()
viewNest' panePath group = do
    activeNotebook  <- getNotebook' "viewNest' 1" panePath
    parent          <- widgetGetParent activeNotebook
    layout          <- getLayoutSt
    let paneLayout  =  layoutFromPath panePath layout
    case paneLayout of
        TerminalP {} -> do
            nb <- newNotebook (panePath ++ [GroupP group])
            widgetSetName nb (groupPrefix <> group)
            notebookInsertOrdered activeNotebook nb group noLabel True
            widgetShowAll nb
                --widgetGrabFocus activeNotebook
            handleFunc <-  runInIO (handleNotebookSwitch nb)
            afterNotebookSwitchPage nb (\w i -> handleFunc $ fromIntegral i)
            adjustLayoutForNest group panePath
        _ -> return ()

closeGroup :: PaneMonad alpha => Text -> alpha ()
closeGroup groupName = do
    layout <- getLayout
    let mbPath = findGroupPath groupName layout
    mainWindow <- getMainWindow
    case mbPath of
        Nothing -> trace ("ViewFrame>>closeGroup: Group path not found: " <> T.unpack groupName) return ()
        Just path -> do
            panesMap <- getPaneMapSt
            let nameAndpathList  = filter (\(a,pp) -> path `isPrefixOf` pp)
                            $ map (second fst) (Map.assocs panesMap)
            continue <- case nameAndpathList of
                            (_:_) -> do
                                md <- new' MessageDialog [
                                    constructDialogUseHeaderBar 1,
                                    constructMessageDialogButtons ButtonsTypeYesNo]
                                setMessageDialogMessageType md MessageTypeQuestion
                                setMessageDialogText md $ "Group " <> groupName <> " not empty. Close with all contents?"
                                windowSetTransientFor md (Just mainWindow)
                                rid <- dialogRun md
                                widgetDestroy md
                                case toEnum $ fromIntegral rid of
                                    ResponseTypeYes ->  return True
                                    otherwise       ->  return False
                            []  -> return True
            when continue $ do
                panes <- mapM (paneFromName . fst) nameAndpathList
                results <- mapM (\ (PaneC p) -> closePane p) panes
                when (and results) $ do
                    nbOrPaned  <- getNotebookOrPaned path return
                    parent <- nullToNothing (widgetGetParent nbOrPaned) >>= liftIO. unsafeCastTo Container . fromJust
                    containerRemove parent nbOrPaned
                    setLayoutSt (removeGL path layout)
                    ppMap <- getPanePathFromNB
                    setPanePathFromNB (Map.filter (\pa -> not (path `isPrefixOf` pa)) ppMap)

viewDetach :: PaneMonad alpha => alpha (Maybe (Window, Notebook))
viewDetach = do
    id <- liftIO $ show <$> getCPUTime
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return Nothing
        Just panePath -> viewDetach' panePath (T.pack id)

viewDetach' :: PaneMonad alpha => PanePath -> Text -> alpha (Maybe (Window, Notebook))
viewDetach' panePath id = do
    activeNotebook  <- getNotebook' "viewDetach'" panePath
    parent <- nullToNothing (widgetGetParent activeNotebook) >>= liftIO . unsafeCastTo Container . fromJust
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath panePath layout
    case paneLayout of
        TerminalP{detachedSize = size} -> do
            window <- windowNew WindowTypeToplevel
            setWindowTitle window "Leksah detached window"
            setWidgetName window id
            case size of
                Just (width, height) -> windowSetDefaultSize window (fromIntegral width) (fromIntegral height)
                Nothing -> do
                    a <- widgetGetAllocation activeNotebook
                    curWidth <- getRectangleWidth a
                    curHeight <- getRectangleHeight a
                    windowSetDefaultSize window curWidth curHeight
            containerRemove parent activeNotebook
            containerAdd window activeNotebook
            widgetShowAll window
            handleFunc <- runInIO (handleReattach id window)
            onWidgetDeleteEvent window $ \e -> handleFunc ()
            windows <- getWindowsSt
            setWindowsSt $ windows ++ [window]
            adjustLayoutForDetach id panePath
            return (Just (window, activeNotebook))
        _ -> return Nothing

handleReattach :: PaneMonad alpha => Text -> Window -> () -> alpha Bool
handleReattach windowId window _ = do
    layout <- getLayout
    case findDetachedPath windowId layout of
        Nothing -> trace ("ViewFrame>>handleReattach: panePath for id not found: " <> T.unpack windowId)
                $ do
            windows <- getWindowsSt
            setWindowsSt $ deleteBy equalManagedPtr window windows
            return False
        Just pp -> do
            nb      <- getNotebook' "handleReattach" pp
            parent  <- getNotebookOrPaned (init pp) (unsafeCastTo Container)
            containerRemove window nb
            containerAdd parent nb
            adjustLayoutForReattach pp
            windows <- getWindowsSt
            setWindowsSt $ deleteBy equalManagedPtr window windows
            case last pp of
                GroupP groupName -> do
                    label <- groupLabel groupName
                    parentNotebook <- liftIO $ unsafeCastTo Notebook parent
                    notebookSetTabLabel parentNotebook nb (Just label)
                otherwise       -> return ()
            return False -- "now destroy the window"


getActiveWindow :: PaneMonad alpha => alpha (Maybe Window)
getActiveWindow = do
    mbPanePath <- getActivePanePath
    case mbPanePath of
        Nothing -> return Nothing
        Just panePath -> do
            activeNotebook  <- getNotebook' "getActiveWindow" panePath
            widgetGetToplevel activeNotebook >>= liftIO . castTo Window

getActiveScreen :: PaneMonad alpha => alpha (Maybe Screen)
getActiveScreen = do
    mbWindow <- getActiveWindow
    case mbWindow of
        Nothing -> return Nothing
        Just window -> Just <$> windowGetScreen window

groupMenuLabel :: PaneMonad beta => Text -> beta (Maybe Label)
groupMenuLabel group = liftM Just (labelNew (Just group))

handleNotebookSwitch :: PaneMonad beta => Notebook -> Int -> beta ()
handleNotebookSwitch nb index =
    nullToNothing (notebookGetNthPage nb (fromIntegral index)) >>= \case
        Nothing -> error "ViewFrame/handleNotebookSwitch: Can't find widget"
        Just w  -> do
            name   <- widgetGetName w
            mbPane <- findPaneFor name
            case mbPane of
                Nothing         ->  return ()
                Just (PaneC p)  ->  makeActive p
  where
        findPaneFor :: PaneMonad beta => Text -> beta (Maybe (IDEPane beta))
        findPaneFor n1   =   do
            panes'      <-  getPanesSt
            foldM (\r (PaneC p) -> do
                n2 <- widgetGetName =<< getTopWidget p
                return (if n1 == n2 then Just (PaneC p) else r))
                        Nothing (Map.elems panes')


--
-- | Moves the activePane in the given direction, if possible
-- | If their are many possibilities choose the leftmost and topmost
--
viewMove :: PaneMonad beta => PaneDirection -> beta  ()
viewMove direction = do
    mbPane <- getActivePaneSt
    case mbPane of
        Nothing -> return ()
        Just (paneName,_) -> do
            (PaneC pane) <- paneFromName paneName
            mbPanePath <- getActivePanePath
            case mbPanePath of
                Nothing -> return ()
                Just panePath -> do
                  layout <- getLayoutSt
                  case findMoveTarget panePath layout direction of
                      Nothing -> return ()
                      Just moveTo -> viewMoveTo moveTo pane

--
-- | Find the target for a move
--
findMoveTarget :: PanePath -> PaneLayout -> PaneDirection -> Maybe PanePath
findMoveTarget panePath layout direction=
    let oppositeDir          = otherDirection direction
        canMove []           = []
        canMove reversedPath =
            case head reversedPath of
                SplitP d | d == oppositeDir
                    -> SplitP direction : tail reversedPath
                GroupP group -> []
                _                     -> canMove (tail reversedPath)
        basePath = reverse (canMove $ reverse panePath)
    in case basePath of
        [] -> Nothing
        _  -> let layoutP  = layoutFromPath basePath layout
             in  Just $basePath ++ findAppropriate layoutP oppositeDir

--
-- | Moves the given Pane to the given path
--
viewMoveTo ::  RecoverablePane alpha beta delta => PanePath -> alpha -> delta ()
viewMoveTo toPanePath pane = do
    let name    = paneName pane
    toNB        <- getNotebook' "move" toPanePath
    move' (name,toNB)

--
-- | Moves the given Pane to the given path, care for groups (layout, paneMap)
--
move' :: PaneMonad alpha => (PaneName,Notebook) -> alpha ()
move' (paneName,toNB) = do
    paneMap         <-  getPaneMapSt
    panes           <-  getPanesSt
    layout          <-  getLayout
    frameState      <-  getFrameState
    case groupPrefix `T.stripPrefix` paneName of
        Just group  ->
            case findGroupPath group layout of
                Nothing -> trace ("ViewFrame>>move': group not found: " <> T.unpack group) return ()
                Just fromPath -> do
                    groupNBOrPaned <- getNotebookOrPaned fromPath return
                    fromNB  <- getNotebook' "move'" (init fromPath)
                    case unsafeManagedPtrCastPtr toNB `Map.lookup` panePathFromNB frameState of
                        Nothing -> trace "ViewFrame>>move': panepath for Notebook not found1" return ()
                        Just toPath ->
                            when (not (fromNB `equalManagedPtr` toNB || fromPath `isPrefixOf` toPath)) $ do
                                num <- notebookPageNum fromNB groupNBOrPaned
                                if num < 0
                                    then trace "ViewFrame>>move': group notebook not found" return ()
                                    else do
                                        notebookRemovePage fromNB num
                                        label <- groupLabel group
                                        notebookInsertOrdered toNB groupNBOrPaned group noLabel True
                                        notebookSetTabLabel toNB groupNBOrPaned (Just label)
                                        adjustPanes fromPath (toPath ++ [GroupP group])
                                        adjustLayoutForGroupMove fromPath toPath group
                                        adjustNotebooks fromPath (toPath ++ [GroupP group])
                                        layout2          <-  getLayout
                                        return ()
        Nothing     ->
            case paneName `Map.lookup` panes of
                Nothing -> trace ("ViewFrame>>move': pane not found: " <> T.unpack paneName) return ()
                Just (PaneC pane) ->
                    case unsafeManagedPtrCastPtr toNB `Map.lookup` panePathFromNB frameState of
                        Nothing -> trace "ViewFrame>>move': panepath for Notebook not found2" return ()
                        Just toPath ->
                            case paneName `Map.lookup`paneMap of
                                Nothing -> trace ("ViewFrame>>move': pane data not found: " <> T.unpack paneName)
                                            return ()
                                Just (fromPath,_) -> do
                                    child           <-  getTopWidget pane
                                    (fromPane,cid)  <-  guiPropertiesFromName paneName
                                    fromNB          <-  getNotebook' "move'" fromPane
                                    when (not (fromNB `equalManagedPtr` toNB)) $ do
                                        num <- notebookPageNum fromNB child
                                        if num < 0
                                            then trace "ViewFrame>>move': widget not found" return ()
                                            else do
                                                notebookRemovePage fromNB num
                                                notebookInsertOrdered toNB child paneName noLabel False
                                                let paneMap1    =   Map.delete paneName paneMap
                                                setPaneMapSt    $   Map.insert paneName (toPath,cid) paneMap1

findAppropriate :: PaneLayout -> PaneDirection -> PanePath
findAppropriate  (TerminalP {}) _ =   []
findAppropriate  (HorizontalP t b _) LeftP     =   SplitP TopP    :  findAppropriate t LeftP
findAppropriate  (HorizontalP t b _) RightP    =   SplitP TopP    :  findAppropriate t RightP
findAppropriate  (HorizontalP t b _) BottomP   =   SplitP BottomP :  findAppropriate b BottomP
findAppropriate  (HorizontalP t b _) TopP      =   SplitP TopP    :  findAppropriate b TopP
findAppropriate  (VerticalP l r _) LeftP       =   SplitP LeftP   :  findAppropriate l LeftP
findAppropriate  (VerticalP l r _) RightP      =   SplitP RightP  :  findAppropriate r RightP
findAppropriate  (VerticalP l r _) BottomP     =   SplitP LeftP   :  findAppropriate l BottomP
findAppropriate  (VerticalP l r _) TopP        =   SplitP LeftP   :  findAppropriate l TopP

--
-- | Bring the pane to the front position in its notebook
--
bringPaneToFront :: RecoverablePane alpha beta delta => alpha -> delta ()
bringPaneToFront pane = do
    tv <- getTopWidget pane
    w <- widgetGetToplevel tv
    visible <- getWidgetVisible w
    when visible $ liftIO (unsafeCastTo Window w) >>= windowPresent
    setCurrentNotebookPages tv


setCurrentNotebookPages :: (MonadIO m, IsWidget widget) => widget -> m ()
setCurrentNotebookPages widget = do
    mbParent <- nullToNothing $ widgetGetParent widget
    case mbParent of
        Just parent -> do
            setCurrentNotebookPages parent
            liftIO (castTo Notebook parent) >>= \case
                Just notebook ->
                    notebookPageNum notebook widget >>= \case
                        -1 -> return ()
                        pageNum -> notebookSetCurrentPage notebook pageNum
                Nothing -> return ()
        Nothing -> return ()

--
-- | Get a valid panePath from a standard path.
--
getBestPanePath :: StandardPath -> PaneLayout -> PanePath
getBestPanePath sp pl = reverse $ getStandard' sp pl []
    where
    getStandard' (GroupP group:sp) (TerminalP {paneGroups = groups}) p
        | group `Map.member` groups                 =   getStandard' sp (groups Map.! group) (GroupP group:p)
    getStandard' _ (TerminalP {}) p              =   p
    getStandard' (SplitP LeftP:sp) (VerticalP l r _) p     =   getStandard' sp l (SplitP LeftP:p)
    getStandard' (SplitP RightP:sp) (VerticalP l r _) p    =   getStandard' sp r (SplitP RightP:p)
    getStandard' (SplitP TopP:sp) (HorizontalP t b _) p    =   getStandard' sp t (SplitP TopP:p)
    getStandard' (SplitP BottomP:sp) (HorizontalP t b _) p =   getStandard' sp b (SplitP BottomP:p)
    -- if no match get leftmost topmost
    getStandard' _ (VerticalP l r _) p              =   getStandard' [] l (SplitP LeftP:p)
    getStandard' _ (HorizontalP t b _) p            =   getStandard' [] t (SplitP TopP:p)

--
-- | Get a standard path.
--
getBestPathForId :: PaneMonad alpha => Text -> alpha PanePath
getBestPathForId  id = do
    p <- panePathForGroup id
    l <- getLayout
    return (getBestPanePath p l)

--
-- | Construct a new notebook
--
newNotebook' :: IO Notebook
newNotebook' = do
    nb <- notebookNew
    widgetSetSizeRequest nb 50 50
    notebookSetTabPos nb PositionTypeTop
    notebookSetShowTabs nb True
    notebookSetScrollable nb True
    setNotebookEnablePopup nb True
    return nb

--
-- | Construct a new notebook,
--
newNotebook :: PaneMonad alpha => PanePath -> alpha Notebook
newNotebook pp = do
    st  <- getFrameState
    nb  <- liftIO newNotebook'
    setPanePathFromNB $ Map.insert (unsafeManagedPtrCastPtr nb) pp (panePathFromNB st)
    func <- runInIO move'
    tl <- targetListNew Nothing
    targetListAddTextTargets tl 0
    widgetDragDestSet nb [DestDefaultsAll] Nothing [DragActionCopy, DragActionMove]
    widgetDragDestSetTargetList nb $ Just tl
    onWidgetDragDataReceived nb (dragFunc nb func)
    return nb
  where
        dragFunc ::
            Notebook ->
            ((PaneName,Notebook) -> IO ()) ->
            DragContext ->
            Int32 ->
            Int32 ->
            SelectionData ->
            Word32 ->
            Word32 ->
            IO ()
        dragFunc nb func cont x y data_ id timeStamp =
            selectionDataGetText data_ >>= \case
                Nothing  -> return ()
                Just str -> do
                    trace ("dragFunc str=" <> T.unpack str) $ return ()
                    func (str,nb)

terminalsWithPanePath :: PaneLayout -> [(PanePath,PaneLayout)]
terminalsWithPanePath pl = map (first reverse) $ terminalsWithPP [] pl
    where
        terminalsWithPP pp t@(TerminalP groups _ _ _ _) =  (pp, t) : concatMap (terminalsFromGroup pp)
                                                                               (Map.toList groups)
        terminalsWithPP pp (VerticalP l r _)       =  terminalsWithPP (SplitP LeftP : pp) l
                                                        ++ terminalsWithPP (SplitP RightP : pp) r
        terminalsWithPP pp (HorizontalP t b _)     =  terminalsWithPP (SplitP TopP : pp) t
                                                        ++ terminalsWithPP (SplitP BottomP : pp) b
        terminalsFromGroup pp (name,layout)        =  terminalsWithPP (GroupP name : pp) layout

findGroupPath :: Text -> PaneLayout -> Maybe PanePath
findGroupPath group layout =
    let terminalPairs = terminalsWithPanePath layout
    in case filter filterFunc terminalPairs of
        [] -> Nothing
        [(pp, _)] -> Just (pp ++ [GroupP group])
        _ -> error ("ViewFrame>>group name not unique: " ++ T.unpack group)
    where
        filterFunc (_, TerminalP groups _ _ _ _) =  group  `Set.member` Map.keysSet groups
        filterFunc _                             =  error "ViewFrame>>findGroupPath: impossible"

findDetachedPath :: Text -> PaneLayout -> Maybe PanePath
findDetachedPath id layout =
    let terminalPairs = terminalsWithPanePath layout
    in case filter filterFunc terminalPairs of
        [] -> Nothing
        [(pp, _)] -> Just pp
        _ -> error ("ViewFrame>>window id not unique: " ++ T.unpack id)
    where
        filterFunc (_, TerminalP _ _ _ (Just lid) _) = lid == id
        filterFunc _                                 = False


allGroupNames :: PaneLayout -> Set Text
allGroupNames pl = Set.unions $ map getFunc (terminalsWithPanePath pl)
    where
        getFunc (_, TerminalP groups _ _ _ _) =  Map.keysSet groups
        getFunc _                             =  error "ViewFrame>>allGroupNames: impossible"


--
-- | Get another pane path which points to the other side at the same level
--
otherSide :: PanePath -> Maybe PanePath
otherSide []    =   Nothing
otherSide p     =   let rp = reverse p
                    in case head rp of
                        SplitP d -> Just (reverse $ SplitP (otherDirection d) : tail rp)
                        _        -> Nothing

--
-- | Get the opposite direction of a pane direction
--
otherDirection :: PaneDirection -> PaneDirection
otherDirection LeftP    = RightP
otherDirection RightP   = LeftP
otherDirection TopP     = BottomP
otherDirection BottomP  = TopP

--
-- | Get the layout at the given pane path
--
layoutFromPath :: PanePath -> PaneLayout -> PaneLayout
layoutFromPath [] l                                   = l
layoutFromPath (GroupP group:r) (TerminalP {paneGroups = groups})
    | group `Map.member` groups                       = layoutFromPath r (groups Map.! group)
layoutFromPath (SplitP TopP:r) (HorizontalP t _ _)    = layoutFromPath r t
layoutFromPath (SplitP BottomP:r) (HorizontalP _ b _) = layoutFromPath r b
layoutFromPath (SplitP LeftP:r) (VerticalP l _ _)     = layoutFromPath r l
layoutFromPath (SplitP RightP:r) (VerticalP _ ri _)   = layoutFromPath r ri
layoutFromPath pp l                                   = error
    $"inconsistent layout (layoutFromPath) " ++ show pp ++ " " ++ show l

layoutsFromPath :: PanePath -> PaneLayout -> [PaneLayout]
layoutsFromPath (GroupP group:r) layout@(TerminalP {paneGroups = groups})
    | group `Map.member` groups
        = layout:layoutsFromPath r (groups Map.! group)
layoutsFromPath [] layout                                     =   [layout]
layoutsFromPath (SplitP TopP:r) layout@(HorizontalP t b _)    =   layout:layoutsFromPath r t
layoutsFromPath (SplitP BottomP:r) layout@(HorizontalP t b _) =   layout:layoutsFromPath r b
layoutsFromPath (SplitP LeftP:r) layout@(VerticalP l ri _)    =   layout:layoutsFromPath r l
layoutsFromPath (SplitP RightP:r) layout@(VerticalP l ri _)   =   layout:layoutsFromPath r ri
layoutsFromPath pp l                                      = error
    $"inconsistent layout (layoutsFromPath) " ++ show pp ++ " " ++ show l

getWidgetNameList :: PanePath -> PaneLayout -> [Text]
getWidgetNameList path layout = reverse $ nameList (reverse path) (reverse $ layoutsFromPath path layout)
    where
        nameList [] _ = reverse ["Leksah Main Window","topBox","root"]
        nameList (pe:_) (TerminalP{detachedId = Just id}:_) = [panePathElementToWidgetName pe, id]
        nameList (pe:rpath) (_:rlayout) = panePathElementToWidgetName pe : nameList rpath rlayout
        nameList _ _ = error $ "inconsistent layout (getWidgetNameList) " ++ show path ++ " " ++ show layout

getNotebookOrPaned :: PaneMonad alpha => PanePath -> (Widget -> IO beta) -> alpha beta
getNotebookOrPaned p cf = do
    layout <- getLayout
    (widgetGet $ getWidgetNameList p layout) cf

--
-- | Get the notebook widget for the given pane path
--
getNotebook :: PaneMonad alpha => PanePath -> alpha  Notebook
getNotebook p = getNotebookOrPaned p (unsafeCastTo Notebook)

getNotebook' :: PaneMonad alpha => Text -> PanePath -> alpha  Notebook
getNotebook' str p = getNotebookOrPaned p (unsafeCastTo Notebook)


--
-- | Get the (gtk) Paned widget for a given path
--
getPaned :: PaneMonad alpha => PanePath -> alpha Paned
getPaned p = getNotebookOrPaned p $ unsafeCastTo Paned

--
-- | Get the path to the active pane
--
getActivePanePath :: PaneMonad alpha => alpha  (Maybe PanePath)
getActivePanePath = do
    mbPane   <- getActivePaneSt
    case mbPane of
        Nothing -> return Nothing
        Just (paneName,_) -> do
            (pp,_)  <- guiPropertiesFromName paneName
            return (Just pp)

getActivePanePathOrStandard :: PaneMonad alpha => StandardPath -> alpha  PanePath
getActivePanePathOrStandard sp = do
    mbApp <- getActivePanePath
    case mbApp of
        Just app -> return app
        Nothing -> do
            layout <- getLayoutSt
            return (getBestPanePath sp layout)

--
-- | Get the active notebook
--
getActiveNotebook :: PaneMonad alpha => alpha  (Maybe Notebook)
getActiveNotebook = do
    mbPanePath <- getActivePanePath
    case mbPanePath of
        Just panePath -> do
            nb <- getNotebook' "getActiveNotebook" panePath
            return (Just nb)
        Nothing -> return Nothing


--
-- | Translates a pane direction to the widget name
--
paneDirectionToWidgetName           :: PaneDirection -> Text
paneDirectionToWidgetName TopP      =  "top"
paneDirectionToWidgetName BottomP   =  "bottom"
paneDirectionToWidgetName LeftP     =  "left"
paneDirectionToWidgetName RightP    =  "right"

panePathElementToWidgetName :: PanePathElement -> Text
panePathElementToWidgetName (SplitP dir)   = paneDirectionToWidgetName dir
panePathElementToWidgetName (GroupP group) = groupPrefix <> group

--
-- | Changes a pane path in the pane map
--
adjustPanes :: PaneMonad alpha => PanePath -> PanePath -> alpha ()
adjustPanes fromPane toPane  = do
    paneMap     <- getPaneMapSt
    setPaneMapSt (Map.map (\(pp,other) ->
        case stripPrefix fromPane pp of
            Just rest -> (toPane ++ rest,other)
            _         -> (pp,other)) paneMap)

adjustNotebooks :: PaneMonad alpha => PanePath -> PanePath -> alpha ()
adjustNotebooks fromPane toPane  = do
    npMap <- trace ("+++ adjustNotebooks from: " <> show fromPane <> " to " <> show toPane)
                getPanePathFromNB
    setPanePathFromNB  (Map.map (\pp ->
        case stripPrefix fromPane pp of
            Just rest -> toPane ++ rest
            _         -> pp) npMap)

--
-- | Changes the layout for a split
--
adjustLayoutForSplit :: PaneMonad alpha => PaneDirection -> PanePath -> alpha ()
adjustLayoutForSplit  dir path  = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newLayout   =   TerminalP Map.empty Nothing 0 Nothing Nothing
        newTerm     =   case dir of
                            LeftP   -> VerticalP paneLayout newLayout 0
                            RightP  -> VerticalP newLayout paneLayout 0
                            TopP    -> HorizontalP paneLayout newLayout 0
                            BottomP -> HorizontalP newLayout paneLayout 0
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a nest
--
adjustLayoutForNest :: PaneMonad alpha => Text -> PanePath -> alpha ()
adjustLayoutForNest group path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {paneGroups = groups}) -> paneLayout {
                                paneGroups = Map.insert group (TerminalP Map.empty Nothing 0 Nothing Nothing) groups}
                            _          -> error "Unexpected layout type in adjustLayoutForNest"
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a detach
--
adjustLayoutForDetach :: PaneMonad alpha => Text -> PanePath -> alpha ()
adjustLayoutForDetach id path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {}) -> paneLayout {detachedId = Just id}
                            _              -> error "Unexpected layout type in adjustLayoutForDetach"
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a reattach
--
adjustLayoutForReattach :: PaneMonad alpha => PanePath -> alpha ()
adjustLayoutForReattach path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {}) -> paneLayout {detachedId = Nothing, detachedSize = Nothing}
                            _   -> error "Unexpected layout type in adjustLayoutForReattach"
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a collapse
--
adjustLayoutForCollapse :: PaneMonad alpha => PanePath -> alpha ()
adjustLayoutForCollapse oldPath  = do
    layout          <-  getLayoutSt
    let pathLayout  =   layoutFromPath oldPath layout
    setLayoutSt     $   adjustLayout (init oldPath) layout pathLayout

--
-- | Changes the layout for a move
--
adjustLayoutForGroupMove :: PaneMonad alpha => PanePath -> PanePath -> Text -> alpha ()
adjustLayoutForGroupMove fromPath toPath group = do
    layout <- getLayout
    let layoutToMove = layoutFromPath fromPath layout
    let newLayout = removeGL fromPath layout
    setLayoutSt (addGL layoutToMove (toPath ++ [GroupP group])  newLayout)

--
-- | Changes the layout for a remove
--
adjustLayoutForGroupRemove :: PaneMonad alpha => PanePath -> Text -> alpha ()
adjustLayoutForGroupRemove fromPath group = do
    layout <- getLayout
    setLayoutSt (removeGL fromPath layout)

--
-- | Remove group layout at a certain path
--
removeGL :: PanePath -> PaneLayout -> PaneLayout
removeGL [GroupP group] t@(TerminalP oldGroups _ _ _ _)
    | group `Map.member` oldGroups                        =  t{paneGroups = group `Map.delete` oldGroups}
removeGL (GroupP group:r)  old@(TerminalP {paneGroups = groups})
    | group `Map.member` groups                             = old{paneGroups = Map.adjust (removeGL r) group groups}
removeGL (SplitP TopP:r)  (HorizontalP tp bp _)     = HorizontalP (removeGL r tp) bp 0
removeGL (SplitP BottomP:r)  (HorizontalP tp bp _)  = HorizontalP tp (removeGL r bp) 0
removeGL (SplitP LeftP:r)  (VerticalP lp rp _)      = VerticalP (removeGL r lp) rp 0
removeGL (SplitP RightP:r)  (VerticalP lp rp _)     = VerticalP lp (removeGL r rp) 0
removeGL p l = error $"ViewFrame>>removeGL: inconsistent layout " ++ show p ++ " " ++ show l

--
-- | Add group layout at a certain path
--
addGL :: PaneLayout -> PanePath -> PaneLayout -> PaneLayout
addGL toAdd [GroupP group] t@(TerminalP oldGroups _ _ _ _)  =  t{paneGroups = Map.insert group toAdd oldGroups}
addGL toAdd (GroupP group:r)  old@(TerminalP {paneGroups = groups})
    | group `Map.member` groups = old{paneGroups       = Map.adjust (addGL toAdd r) group groups}
addGL toAdd (SplitP TopP:r)  (HorizontalP tp bp _)     = HorizontalP (addGL toAdd r tp) bp 0
addGL toAdd (SplitP BottomP:r)  (HorizontalP tp bp _)  = HorizontalP tp (addGL toAdd r bp) 0
addGL toAdd (SplitP LeftP:r)  (VerticalP lp rp _)      = VerticalP (addGL toAdd r lp) rp 0
addGL toAdd (SplitP RightP:r)  (VerticalP lp rp _)     = VerticalP lp (addGL toAdd r rp) 0
addGL _ p l = error $"ViewFrame>>addGL: inconsistent layout" ++ show p ++ " " ++ show l

--
-- | Changes the layout by replacing element at pane path (pp) with replace
--
adjustLayout :: PanePath -> PaneLayout -> PaneLayout -> PaneLayout
adjustLayout pp layout replace    = adjust' pp layout
    where
    adjust' [] _                                       = replace
    adjust' (GroupP group:r)  old@(TerminalP {paneGroups = groups})
        | group `Map.member` groups =
            old{paneGroups = Map.adjust (adjustPaneGroupLayout r) group groups}
    adjust' (SplitP TopP:r)  (HorizontalP tp bp _)     = HorizontalP (adjust' r tp) bp 0
    adjust' (SplitP BottomP:r)  (HorizontalP tp bp _)  = HorizontalP tp (adjust' r bp) 0
    adjust' (SplitP LeftP:r)  (VerticalP lp rp _)      = VerticalP (adjust' r lp) rp 0
    adjust' (SplitP RightP:r)  (VerticalP lp rp _)     = VerticalP lp (adjust' r rp) 0
    adjust' p l = error $"inconsistent layout (adjust) " ++ show p ++ " " ++ show l
    adjustPaneGroupLayout = adjust'

--
-- | Get the widget from a list of strings
--
widgetFromPath :: MonadIO m => Widget -> [Text] -> m Widget
widgetFromPath w [] = return w
widgetFromPath w path = do
    children    <- liftIO (unsafeCastTo Container w) >>= containerGetChildren
    chooseWidgetFromPath children path

chooseWidgetFromPath :: MonadIO m => [Widget] -> [Text] -> m Widget
chooseWidgetFromPath _ [] = error "Cant't find widget (empty path)"
chooseWidgetFromPath widgets (h:t) = do
    names       <- mapM widgetGetName widgets
    let mbiInd  =  elemIndex h names
    case mbiInd of
        Nothing     -> error $"Cant't find widget path " ++ show (h:t) ++ " found only " ++ show names
        Just ind    -> widgetFromPath (widgets !! ind) t

widgetGet :: PaneMonad alpha => [Text] -> (Widget -> IO b) -> alpha  b
widgetGet strL cf = do
    windows <- getWindowsSt
    widgets <- liftIO $ mapM toWidget windows
    r <- liftIO $ chooseWidgetFromPath widgets strL
    liftIO (cf r)

widgetGetRel :: Widget -> [Text] -> (Widget -> b) -> IO b
widgetGetRel w sl cf = do
    r <- widgetFromPath w sl
    return (cf r)

getUIAction :: PaneMonad alpha => Text -> (Action -> IO a) -> alpha a
getUIAction str f = do
    uiManager <- getUiManagerSt
    findAction <- nullToNothing $ uIManagerGetAction uiManager str
    case findAction of
        Just act -> liftIO $ f act
        Nothing  -> error $"getUIAction can't find action " ++ T.unpack str

getThis :: PaneMonad delta =>  (FrameState delta -> alpha) -> delta alpha
getThis sel = do
    st <- getFrameState
    return (sel st)
setThis :: PaneMonad delta =>  (FrameState delta -> alpha -> FrameState delta) -> alpha -> delta ()
setThis sel value = do
    st <- getFrameState
    trace ("!!! setFrameState " <> show (sel st value)) $ setFrameState (sel st value)

getWindowsSt    = getThis windows
setWindowsSt    = setThis (\st value -> st{windows = value})
getUiManagerSt  = getThis uiManager
getPanesSt      = getThis panes
setPanesSt      = setThis (\st value -> st{panes = value})
getPaneMapSt    = getThis paneMap
setPaneMapSt    = setThis (\st value -> st{paneMap = value})
getActivePaneSt = getThis activePane
setActivePaneSt = setThis (\st value -> st{activePane = value})
getLayoutSt     = getThis layout
setLayoutSt     = setThis (\st value -> st{layout = value})
getPanePathFromNB  = getThis panePathFromNB
setPanePathFromNB  = setThis (\st value -> st{panePathFromNB = value})

getActivePane   = getActivePaneSt
setActivePane   = setActivePaneSt
getUiManager    = getUiManagerSt
getWindows      = getWindowsSt
getMainWindow   = liftM head getWindows
getLayout       = getLayoutSt

