{-# LANGUAGE CPP #-}
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
{-# OPTIONS_GHC -Wall #-}
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
,   getMRUPanes

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
import Control.Applicative (Applicative, (<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (findIndex, isPrefixOf, deleteBy, stripPrefix, elemIndex)
import Data.Maybe
import Data.Typeable
import Data.Text (Text)

import Graphics.UI.Frame.Panes
import Graphics.UI.Editor.Parameters
import System.CPUTime (getCPUTime)
import Graphics.UI.Editor.MakeEditor
    (mkField, FieldDescription(..), buildEditor)
import Graphics.UI.Editor.Simple (textEditor)
import qualified Data.Set as  Set (unions, member)
import Data.Set (Set)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (unless, when, foldM, void)
import qualified Data.Text as T (pack, stripPrefix, unpack)
import Data.Foldable (forM_)
import Control.Arrow (Arrow(..))
import GI.Gtk
       (UIManager, panedNew, gridNew, widgetSetValign, widgetSetHalign,
        windowGetScreen, Bin(..), uIManagerGetAction, Action, toWidget,
        selectionDataGetText, SelectionData, onWidgetDragDataReceived,
        widgetDragDestSetTargetList, widgetDragDestSet,
        setNotebookEnablePopup, notebookSetScrollable,
        widgetSetSizeRequest, notebookNew, windowPresent, getWidgetVisible,
        notebookRemovePage, Window(..), widgetGetToplevel,
        onWidgetDeleteEvent, widgetGetAllocation, windowSetDefaultSize,
        setWidgetName, windowNew, Widget(..), dialogGetContentArea,
        setWindowTitle, setWindowTransientFor, Window,
        windowSetTransientFor, setMessageDialogText,
        constructDialogUseHeaderBar, MessageDialog(..), widgetDestroy,
        dialogRun, afterNotebookSwitchPage, widgetGrabFocus,
        boxReorderChild, Box(..), notebookSetMenuLabel,
        notebookSetTabLabel, notebookInsertPage, Paned(..), panedPack1,
        containerRemove, notebookPageNum, Notebook(..), panedPack2,
        Container(..), widgetGetParent, notebookGetCurrentPage,
        notebookSetTabPos, PositionType(..), notebookSetShowTabs,
        notebookGetShowTabs, Label(..), containerGetChildren, binGetChild,
        notebookGetTabLabel, labelSetMarkup, labelSetUseMarkup,
        onWidgetButtonReleaseEvent, onButtonClicked, selectionDataSetText,
        onWidgetDragDataGet, widgetDragSourceSetTargetList,
        targetListAddTextTargets, targetListNew, widgetDragSourceSet,
        setWidgetHalign, setWidgetValign, cssProviderLoadFromData,
        boxPackStart, containerAdd, containerSetBorderWidth,
        styleContextAddProvider, widgetGetStyleContext, cssProviderNew,
        imageNewFromPixbuf, iconThemeLoadIcon, iconThemeGetDefault,
        buttonSetRelief, buttonNew, eventBoxSetVisibleWindow, eventBoxNew,
        EventBox, notebookSetCurrentPage, widgetShowAll,
        notebookInsertPageMenu, widgetGetName, notebookGetNthPage,
        notebookGetNPages, labelNew, Label, IsWidget, IsNotebook,
        widgetSetName)
#ifdef MIN_VERSION_GTK_3_20
import GI.Gtk.Objects.Widget (widgetSetFocusOnClick)
#else
import GI.Gtk.Objects.Button (buttonSetFocusOnClick)
#endif
import GI.Gtk.Enums
       (Orientation(..), WindowType(..), ResponseType(..),
        ButtonsType(..), MessageType(..), Align(..),
        ReliefStyle(..))
import GI.Gtk.Flags (DestDefaults(..), IconLookupFlags(..))
import GI.Gdk.Flags (ModifierType(..), DragAction(..))
import GI.Gdk
       (DragContext, Screen, getEventButtonState)
import Graphics.UI.Frame.Rectangle (getRectangleWidth, getRectangleHeight)
import Data.GI.Base
       (unsafeManagedPtrCastPtr, castTo, unsafeCastTo, new')
import Data.Int (Int32)
import Data.Word (Word32)
import Data.GI.Gtk.ModelView.Types (equalManagedPtr)
import GI.Gtk.Objects.Dialog (Dialog(..))
import GI.Gtk.Objects.MessageDialog
       (constructMessageDialogButtons, setMessageDialogMessageType)
import GI.Gtk.Objects.Label (noLabel)
import GI.Gtk.Objects.Widget (widgetSetTooltipText)
import GHC.Stack (HasCallStack)
import Foreign (Ptr)

-- import Debug.Trace (trace)
trace :: String -> a -> a
trace _ a = a

groupPrefix :: Text
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
    if (paneName pane `Map.notMember` paneMap') &&
       (paneName pane `Map.notMember` panes')
        then do
            setPaneMapSt (Map.insert (paneName pane) (pp, conn) paneMap')
            setPanesSt (Map.insert (paneName pane) (PaneC pane) panes')
            return True
        else trace
               ("ViewFrame>addPaneAdmin:pane with this name already exist" <>
                  T.unpack (paneName pane))
               $ return False

getPanePrim ::  RecoverablePane alpha beta delta => delta (Maybe alpha)
getPanePrim =
    getPanes >>= \case
        [p] -> return $ Just p
        _ -> return Nothing

getPanes ::  RecoverablePane alpha beta delta => delta [alpha]
getPanes = mapMaybe (\ (PaneC p) -> cast p) . Map.elems <$> getPanesSt

notebookInsertOrdered :: PaneMonad alpha => (IsNotebook self, IsWidget child)
    => self
    -> child        -- child - the Widget to use as the contents of the page.
    -> Text
    -> Maybe Label  -- the label for the page as Text or Label
    -> Maybe Text   -- ^ Text for tooltip when hovering
    -> Bool
    -> alpha ()
notebookInsertOrdered nb widget labelStr mbLabel mbTooltipText isGroup = do
    label       <-  case mbLabel of
                        Nothing -> labelNew (Just labelStr)
                        Just l  -> return l
    menuLabel   <-  labelNew (Just labelStr)
    numPages    <-  notebookGetNPages nb
    mbWidgets   <-  mapM (notebookGetNthPage nb) [0 .. (numPages-1)]
    let widgets =   map (fromMaybe (error "ViewFrame.notebookInsertOrdered: no widget")) mbWidgets
    labelStrs   <-  mapM widgetGetName widgets
    let pos     =   fromMaybe (-1)
                      (findIndex
                         (\ s -> withoutGroupPrefix s > withoutGroupPrefix labelStr)
                         labelStrs)
    labelBox    <-  if isGroup then groupLabel labelStr else mkLabelBox label labelStr
    realPos     <-  notebookInsertPageMenu nb widget (Just labelBox) (Just menuLabel) (fromIntegral pos)
    widgetSetTooltipText labelBox mbTooltipText
    widgetShowAll labelBox
    notebookSetCurrentPage nb realPos

-- | Returns a label box
mkLabelBox :: PaneMonad alpha => Label -> Text -> alpha EventBox
mkLabelBox lbl paneName' = do
    widgetSetHalign lbl AlignStart
    widgetSetValign lbl AlignStart

    labelBox  <- eventBoxNew
    eventBoxSetVisibleWindow labelBox False
    innerBox  <- gridNew

    tabButton <- buttonNew
    widgetSetName tabButton "leksah-close-button"
#ifdef MIN_VERSION_GTK_3_20
    widgetSetFocusOnClick tabButton False
#else
    buttonSetFocusOnClick tabButton False
#endif
    buttonSetRelief tabButton ReliefStyleNone
    widgetSetHalign tabButton AlignEnd
    widgetSetValign tabButton AlignCenter

    iconTheme <- iconThemeGetDefault
    image <- iconThemeLoadIcon iconTheme "window-close" 10 [IconLookupFlagsUseBuiltin] >>= imageNewFromPixbuf

    provider <- cssProviderNew
    cssProviderLoadFromData provider (
        ".button {\n" <>
        "padding: 0px;\n" <>
        "border-width: 0px;\n" <>
        "}\n" <>
        "GtkImage {\n" <>
        "padding: 0px;\n" <>
        "}\n")
    context1 <- widgetGetStyleContext tabButton
    styleContextAddProvider context1 provider 600
    context2 <- widgetGetStyleContext image
    styleContextAddProvider context2 provider 600
    setWidgetValign tabButton AlignCenter
    setWidgetValign lbl AlignCenter

    containerSetBorderWidth tabButton 0
    containerAdd tabButton image

    containerAdd innerBox lbl
    containerAdd innerBox tabButton

    containerAdd labelBox innerBox
    setWidgetHalign innerBox AlignCenter
    widgetDragSourceSet labelBox [ModifierTypeButton1Mask] Nothing [DragActionCopy,DragActionMove]
    tl <- targetListNew Nothing
    targetListAddTextTargets tl 0
    widgetDragSourceSetTargetList labelBox $ Just tl
    _ <- onWidgetDragDataGet labelBox $ \ _cont sel _id _timeStamp -> do
        trace ("drag paneName=" <> T.unpack paneName') $ return ()
        void $ selectionDataSetText sel paneName' (-1)
    cl <- runInIO closeHandler
    _ <- onButtonClicked tabButton (cl ())
    _ <- onWidgetButtonReleaseEvent labelBox $ \e -> do
        modifiers <- getEventButtonState e
        let middleButton = ModifierTypeButton2Mask
        when (middleButton `elem` modifiers) (cl ())
        return False

    return labelBox
    where
        closeHandler :: PaneMonad alpha => () -> alpha ()
        closeHandler _ =    case groupPrefix `T.stripPrefix` paneName' of
                                Just group  -> closeGroup group
                                Nothing -> do
                                    (PaneC pane) <- paneFromName paneName'
                                    void $ closePane pane

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
    notebookGetTabLabel nb topWidget >>= \case
        Nothing  -> return ()
        Just box -> liftIO (unsafeCastTo Bin box) >>= binGetChild >>= \case
            Nothing -> return ()
            Just container -> do
                children <- liftIO (unsafeCastTo Container container) >>= containerGetChildren
                label <- liftIO . unsafeCastTo Label $ case children of
                    (_:l:_) -> l
                    _ -> error "ViewFrame>>markLabel: empty children"
                text <- widgetGetName topWidget
                labelSetUseMarkup label True
                labelSetMarkup label
                    (if modified
                          then "<span foreground=\"red\">" <> text <> "</span>"
                      else text)

-- | Constructs a unique pane name, which is an index and a string
figureOutPaneName :: PaneMonad alpha => Text -> alpha (Int,Text)
figureOutPaneName bn = do
    bufs <- getPanesSt
    let ind = foldr (\(PaneC buf) ind' ->
                if primPaneName buf == bn
                    then max ind' (getAddedIndex buf + 1)
                    else ind')
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
mbPaneFromName pn = Map.lookup pn <$> getPanesSt

-- |
guiPropertiesFromName :: PaneMonad alpha => PaneName -> alpha (PanePath, Connections)
guiPropertiesFromName pn =
  fmap (Map.lookup pn) getPaneMapSt >>= \case
    Just it -> return it
    _  -> error $"Cant't find guiProperties from unique name " ++ T.unpack pn

posTypeToPaneDirection :: PositionType -> PaneDirection
posTypeToPaneDirection PositionTypeLeft      =   LeftP
posTypeToPaneDirection PositionTypeRight     =   RightP
posTypeToPaneDirection PositionTypeTop       =   TopP
posTypeToPaneDirection PositionTypeBottom    =   BottomP
posTypeToPaneDirection _ = error "posTypeToPaneDirection"

paneDirectionToPosType :: PaneDirection -> PositionType
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
viewSplitHorizontal     = viewSplit OrientationHorizontal

--
-- | Split the currently active pane in vertical direction
--
viewSplitVertical :: PaneMonad alpha => alpha ()
viewSplitVertical = viewSplit OrientationVertical

--
-- | The active view can be split in two (horizontal or vertical)
--
viewSplit :: PaneMonad alpha => Orientation -> alpha ()
viewSplit orientation = do
    mbPanePath <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> viewSplit' panePath orientation

viewSplit' :: PaneMonad alpha => PanePath -> Orientation -> alpha ()
viewSplit' panePath orientation = do
    l <- getLayout
    case layoutFromPath panePath l of
        (TerminalP _ _ _ (Just _) _) -> trace "ViewFrame>>viewSplit': can't split detached: " return ()
        _                            -> do
            activeNotebook  <- getNotebook' "viewSplit" panePath
            ind <- notebookGetCurrentPage activeNotebook
            parent <- widgetGetParent activeNotebook >>= liftIO . unsafeCastTo Container . fromJust
            let (name,altname,paneDir,
                 oldPath,newPath) = case orientation of
                                        OrientationHorizontal ->
                                            ( "top" :: Text
                                            , "bottom" :: Text
                                            , TopP
                                            , panePath ++ [SplitP TopP]
                                            , panePath ++ [SplitP BottomP])
                                        OrientationVertical ->
                                            ( "left"
                                            , "right"
                                            , LeftP
                                            , panePath ++ [SplitP LeftP]
                                            , panePath ++ [SplitP RightP])
                                        _ -> error "viewSplit'"
            adjustNotebooks panePath oldPath
            frameState  <- getFrameState
            notebookPtr <- liftIO $ unsafeManagedPtrCastPtr activeNotebook
            setPanePathFromNB $ Map.insert notebookPtr oldPath (panePathFromNB frameState)
            nb  <- newNotebook newPath
            newpane <- panedNew $ case orientation of
                  OrientationHorizontal  -> OrientationVertical
                  OrientationVertical    -> OrientationHorizontal
                  _ -> error "viewSplit'"
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
                    _ <- notebookInsertPage parentNotebook newpane (Just label) n
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
            _ <- afterNotebookSwitchPage nb (\_w i -> handleFunc $ fromIntegral i)
            adjustPanes panePath (panePath ++ [SplitP paneDir])
            adjustLayoutForSplit paneDir panePath
            notebookGetNthPage activeNotebook ind >>= \case
                Nothing -> return ()
                Just widget ->
                    widgetGetName widget >>= mbPaneFromName >>= \case
                        Just (PaneC pane) -> viewMoveTo (panePath ++ [SplitP (otherDirection paneDir)]) pane
                        Nothing -> return ()

--
-- | Two notebooks can be collapsed to one
--
viewCollapse :: PaneMonad alpha => alpha ()
viewCollapse = do
    mbPanePath        <- getActivePanePath
    forM_ mbPanePath viewCollapse'

viewCollapse' :: (HasCallStack, PaneMonad alpha) => PanePath -> alpha ()
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
                Just otherSidePath ->
                    getNotebookOrPaned otherSidePath (castTo Notebook) >>= \case
                        Nothing -> trace "ViewFrame>>viewCollapse': other side path not collapsedXX: " $
                            case layoutFromPath otherSidePath layout1 of
                                VerticalP{}   -> do
                                    viewCollapse' (otherSidePath ++ [SplitP LeftP])
                                    viewCollapse' panePath
                                HorizontalP{} -> do
                                    viewCollapse' (otherSidePath ++ [SplitP TopP])
                                    viewCollapse' panePath
                                _ -> trace "ViewFrame>>viewCollapse': impossible1 " return ()
                        Just otherSideNotebook ->
                            getNotebookOrPaned panePath (castTo Notebook) >>= \case
                                Nothing -> trace "ViewFrame>>viewCollapse': path not collapsedXX: " $
                                        case layoutFromPath panePath layout1 of
                                            VerticalP{}   -> do
                                                viewCollapse' (panePath ++ [SplitP LeftP])
                                                viewCollapse' panePath
                                            HorizontalP{} -> do
                                                viewCollapse' (panePath ++ [SplitP TopP])
                                                viewCollapse' panePath
                                            _ -> trace "ViewFrame>>viewCollapse': impossible1 " return ()
                                Just activeNotebook -> do
                                    paneMap'           <- getPaneMapSt
                                    -- 1. Move panes and groups to one side (includes changes to paneMap and layout)
                                    let paneNamesToMove = map fst
                                                            $filter (\(_w,(p,_)) -> otherSidePath == p)
                                                                $Map.toList paneMap'
                                    panesToMove       <- mapM paneFromName paneNamesToMove
                                    mapM_ (\(PaneC p) -> viewMoveTo panePath p) panesToMove
                                    let groupNames    =  map (\n -> groupPrefix <> n) $
                                                                getGroupsFrom otherSidePath layout1
                                    mapM_ (\n -> move' (n,activeNotebook)) groupNames
                                    -- 2. Remove unused notebook from admin
                                    st <- getFrameState
                                    notebookPtr <- liftIO $ unsafeManagedPtrCastPtr otherSideNotebook
                                    let ! newMap = Map.delete notebookPtr (panePathFromNB st)
                                    setPanePathFromNB newMap
                                    -- 3. Remove one level and reparent notebook
                                    parent <- widgetGetParent activeNotebook >>= liftIO . unsafeCastTo Container . fromJust
                                    grandparent <- widgetGetParent parent >>= liftIO . unsafeCastTo Container . fromJust
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
                                                    _ <- notebookInsertPage grandParentNotebook activeNotebook (Just label) n
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
getGroupsFrom path layout' =
    case layoutFromPath path layout' of
        t@TerminalP{} -> Map.keys (paneGroups t)
        HorizontalP{} -> []
        VerticalP{}   -> []

viewNewGroup :: PaneMonad alpha => alpha ()
viewNewGroup = do
    mainWindow <- getMainWindow
    groupNameDialog mainWindow >>= \case
        Just groupName ->
            fmap (Set.member groupName . allGroupNames) getLayoutSt >>= \case
                True -> do
                    md <- new' MessageDialog [
                        constructDialogUseHeaderBar 0,
                        constructMessageDialogButtons ButtonsTypeClose]
                    setMessageDialogMessageType md MessageTypeWarning
                    setMessageDialogText md $ "Group name not unique " <> groupName
                    windowSetTransientFor md (Just mainWindow)
                    _ <- dialogRun md
                    widgetDestroy md
                    return ()
                False -> viewNest groupName
        Nothing -> return ()

newGroupOrBringToFront :: PaneMonad alpha => Text -> PanePath -> alpha (Maybe PanePath,Bool)
newGroupOrBringToFront groupName pp = do
    layout' <- getLayoutSt
    if groupName `Set.member` allGroupNames layout'
        then do
            mbPP <- bringGroupToFront groupName
            return (mbPP,False)
        else do
            let realPath = getBestPanePath pp layout'
            viewNest' realPath groupName
            return (Just (realPath ++ [GroupP groupName]),True)

bringGroupToFront :: PaneMonad alpha => Text -> alpha (Maybe PanePath)
bringGroupToFront groupName =
    fmap (findGroupPath groupName) getLayoutSt >>= \case
        Just path -> do
            widget <- getNotebookOrPaned path return
            setCurrentNotebookPages widget
            return (Just path)
        Nothing -> return Nothing


--  Yet another stupid little dialog

groupNameDialog :: (Applicative m, MonadIO m) => Window -> m (Maybe Text)
groupNameDialog parent = do
    dia                        <-   new' Dialog [constructDialogUseHeaderBar 0]
    setWindowTransientFor dia parent
    setWindowTitle dia "Group"
    upper                      <-   dialogGetContentArea dia >>= liftIO . unsafeCastTo Box
    (widget,_inj,ext,_)         <-   buildEditor fields ""
    _okButton <- dialogAddButton' dia "New" ResponseTypeOk
    boxPackStart upper widget True True 7
    widgetShowAll dia
    resp  <- dialogRun dia
    value <- liftIO $ ext ""
    widgetDestroy dia
    case toEnum $ fromIntegral resp of
        ResponseTypeOk | value /= Just "" -> return value
        _                                 -> return Nothing
  where
        fields :: FieldDescription Text
        fields = VFD emptyParams [
                mkField
                    (paraName <<<- ParaName "Group name "
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
    _parent         <- widgetGetParent activeNotebook
    layout'         <- getLayoutSt
    let paneLayout  =  layoutFromPath panePath layout'
    case paneLayout of
        TerminalP {} -> do
            nb <- newNotebook (panePath ++ [GroupP group])
            widgetSetName nb (groupPrefix <> group)
            notebookInsertOrdered activeNotebook nb group noLabel Nothing True
            widgetShowAll nb
                --widgetGrabFocus activeNotebook
            handleFunc <-  runInIO (handleNotebookSwitch nb)
            _ <- afterNotebookSwitchPage nb (\_w i -> handleFunc $ fromIntegral i)
            adjustLayoutForNest group panePath
        _ -> return ()

closeGroup :: PaneMonad alpha => Text -> alpha ()
closeGroup groupName = do
    layout' <- getLayout
    let mbPath = findGroupPath groupName layout'
    mainWindow <- getMainWindow
    case mbPath of
        Nothing -> trace ("ViewFrame>>closeGroup: Group path not found: " <> T.unpack groupName) return ()
        Just path -> do
            panesMap <- getPaneMapSt
            let nameAndpathList  = filter (\(_a,pp) -> path `isPrefixOf` pp)
                            $ map (second fst) (Map.assocs panesMap)
            continue <- case nameAndpathList of
                            (_:_) -> do
                                md <- new' MessageDialog [
                                    constructDialogUseHeaderBar 0,
                                    constructMessageDialogButtons ButtonsTypeYesNo]
                                setMessageDialogMessageType md MessageTypeQuestion
                                setMessageDialogText md $ "Group " <> groupName <> " not empty. Close with all contents?"
                                windowSetTransientFor md (Just mainWindow)
                                rid <- dialogRun md
                                widgetDestroy md
                                case toEnum $ fromIntegral rid of
                                    ResponseTypeYes ->  return True
                                    _               ->  return False
                            []  -> return True
            when continue $ do
                panes' <- mapM (paneFromName . fst) nameAndpathList
                results <- mapM (\ (PaneC p) -> closePane p) panes'
                when (and results) $ do
                    nbOrPaned  <- getNotebookOrPaned path return
                    parent <- widgetGetParent nbOrPaned >>= liftIO. unsafeCastTo Container . fromJust
                    containerRemove parent nbOrPaned
                    setLayoutSt (removeGL path layout')
                    ppMap <- getPanePathFromNB
                    setPanePathFromNB (Map.filter (\pa -> not (path `isPrefixOf` pa)) ppMap)

viewDetach :: PaneMonad alpha => alpha (Maybe (Window, Notebook))
viewDetach = do
    id' <- liftIO $ show <$> getCPUTime
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return Nothing
        Just panePath -> viewDetach' panePath (T.pack id')

viewDetach' :: PaneMonad alpha => PanePath -> Text -> alpha (Maybe (Window, Notebook))
viewDetach' panePath id' = do
    activeNotebook  <- getNotebook' "viewDetach'" panePath
    parent <- widgetGetParent activeNotebook >>= liftIO . unsafeCastTo Container . fromJust
    fmap (layoutFromPath panePath) getLayoutSt >>= \case
        TerminalP{detachedSize = size} -> do
            window <- windowNew WindowTypeToplevel
            setWindowTitle window "Leksah detached window"
            setWidgetName window id'
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
            handleFunc <- runInIO (handleReattach id' window)
            _ <- onWidgetDeleteEvent window $ \_e -> handleFunc ()
            windows' <- getWindowsSt
            setWindowsSt $ windows' ++ [window]
            adjustLayoutForDetach id' panePath
            return (Just (window, activeNotebook))
        _ -> return Nothing

handleReattach :: PaneMonad alpha => Text -> Window -> () -> alpha Bool
handleReattach windowId window _ =
    fmap (findDetachedPath windowId) getLayout >>= \case
        Nothing -> trace ("ViewFrame>>handleReattach: panePath for id not found: " <> T.unpack windowId)
                $ do
            windows' <- getWindowsSt
            setWindowsSt $ deleteBy equalManagedPtr window windows'
            return False
        Just pp -> do
            nb      <- getNotebook' "handleReattach" pp
            parent  <- getNotebookOrPaned (init pp) (unsafeCastTo Container)
            containerRemove window nb
            containerAdd parent nb
            adjustLayoutForReattach pp
            windows' <- getWindowsSt
            setWindowsSt $ deleteBy equalManagedPtr window windows'
            case last pp of
                GroupP groupName -> do
                    label <- groupLabel groupName
                    parentNotebook <- liftIO $ unsafeCastTo Notebook parent
                    notebookSetTabLabel parentNotebook nb (Just label)
                _ -> return ()
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
groupMenuLabel group = Just <$> labelNew (Just group)

handleNotebookSwitch :: PaneMonad beta => Notebook -> Int -> beta ()
handleNotebookSwitch nb index =
    notebookGetNthPage nb (fromIntegral index) >>= \case
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
viewMove direction =
    getActivePaneSt >>= \case
        (Nothing, _) -> return ()
        (Just (paneName',_),_) -> do
            (PaneC pane) <- paneFromName paneName'
            getActivePanePath >>= \case
                Nothing -> return ()
                Just panePath -> do
                  layout' <- getLayoutSt
                  case findMoveTarget panePath layout' direction of
                      Nothing -> return ()
                      Just moveTo -> viewMoveTo moveTo pane

--
-- | Find the target for a move
--
findMoveTarget :: PanePath -> PaneLayout -> PaneDirection -> Maybe PanePath
findMoveTarget panePath layout' direction=
    let oppositeDir          = otherDirection direction
        canMove []           = []
        canMove (SplitP d:rest) | d == oppositeDir
                    = SplitP direction : rest
        canMove (GroupP _group:_) = []
        canMove (_:rest) = canMove rest
        basePath = reverse (canMove $ reverse panePath)
    in case basePath of
        [] -> Nothing
        _  -> let layoutP  = layoutFromPath basePath layout'
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
move' (paneName', toNB) = do
    paneMap'        <-  getPaneMapSt
    panes'          <-  getPanesSt
    layout'         <-  getLayout
    frameState      <-  getFrameState
    case groupPrefix `T.stripPrefix` paneName' of
        Just group  ->
            case findGroupPath group layout' of
                Nothing -> trace ("ViewFrame>>move': group not found: " <> T.unpack group) return ()
                Just fromPath -> do
                    groupNBOrPaned <- getNotebookOrPaned fromPath return
                    fromNB  <- getNotebook' "move'" (init fromPath)
                    toNBPtr <- liftIO $ unsafeManagedPtrCastPtr toNB
                    case toNBPtr `Map.lookup` panePathFromNB frameState of
                        Nothing -> trace "ViewFrame>>move': panepath for Notebook not found1" return ()
                        Just toPath ->
                            unless (fromNB `equalManagedPtr` toNB || fromPath `isPrefixOf` toPath) $ do
                                num <- notebookPageNum fromNB groupNBOrPaned
                                if num < 0
                                    then trace "ViewFrame>>move': group notebook not found" return ()
                                    else do
                                        notebookRemovePage fromNB num
                                        label <- groupLabel group
                                        notebookInsertOrdered toNB groupNBOrPaned group noLabel Nothing True
                                        notebookSetTabLabel toNB groupNBOrPaned (Just label)
                                        adjustPanes fromPath (toPath ++ [GroupP group])
                                        adjustLayoutForGroupMove fromPath toPath group
                                        adjustNotebooks fromPath (toPath ++ [GroupP group])
                                        _layout2          <-  getLayout
                                        return ()
        Nothing     ->
            case paneName' `Map.lookup` panes' of
                Nothing -> trace ("ViewFrame>>move': pane not found: " <> T.unpack paneName') return ()
                Just (PaneC pane) -> do
                    toNBPtr <- liftIO $ unsafeManagedPtrCastPtr toNB
                    case toNBPtr `Map.lookup` panePathFromNB frameState of
                        Nothing -> trace "ViewFrame>>move': panepath for Notebook not found2" return ()
                        Just toPath ->
                            case paneName' `Map.lookup` paneMap' of
                                Nothing -> trace ("ViewFrame>>move': pane data not found: " <> T.unpack paneName')
                                            return ()
                                Just (_fromPath,_) -> do
                                    child           <-  getTopWidget pane
                                    (fromPane,cid)  <-  guiPropertiesFromName paneName'
                                    fromNB          <-  getNotebook' "move'" fromPane
                                    unless (fromNB `equalManagedPtr` toNB) $ do
                                        num <- notebookPageNum fromNB child
                                        if num < 0
                                            then trace "ViewFrame>>move': widget not found" return ()
                                            else do
                                                notebookRemovePage fromNB num
                                                notebookInsertOrdered toNB child paneName' noLabel (paneTooltipText pane) False
                                                let paneMap1    =   Map.delete paneName' paneMap'
                                                setPaneMapSt    $   Map.insert paneName' (toPath,cid) paneMap1

findAppropriate :: PaneLayout -> PaneDirection -> PanePath
findAppropriate  TerminalP {} _ = []
findAppropriate  (HorizontalP t _b _) LeftP     =   SplitP TopP    :  findAppropriate t LeftP
findAppropriate  (HorizontalP t _b _) RightP    =   SplitP TopP    :  findAppropriate t RightP
findAppropriate  (HorizontalP _t b _) BottomP   =   SplitP BottomP :  findAppropriate b BottomP
findAppropriate  (HorizontalP _t b _) TopP      =   SplitP TopP    :  findAppropriate b TopP
findAppropriate  (VerticalP l _r _) LeftP       =   SplitP LeftP   :  findAppropriate l LeftP
findAppropriate  (VerticalP _l r _) RightP      =   SplitP RightP  :  findAppropriate r RightP
findAppropriate  (VerticalP l _r _) BottomP     =   SplitP LeftP   :  findAppropriate l BottomP
findAppropriate  (VerticalP _l r _) TopP        =   SplitP RightP  :  findAppropriate r TopP

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
    mbParent <- widgetGetParent widget
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
    getStandard' (GroupP group:sp') TerminalP {paneGroups = groups} p
        | group `Map.member` groups                 =   getStandard' sp' (groups Map.! group) (GroupP group:p)
    getStandard' _ TerminalP {} p              =   p
    getStandard' (SplitP LeftP:sp') (VerticalP l _r _) p     =   getStandard' sp' l (SplitP LeftP:p)
    getStandard' (SplitP RightP:sp') (VerticalP _l r _) p    =   getStandard' sp' r (SplitP RightP:p)
    getStandard' (SplitP TopP:sp') (HorizontalP t _b _) p    =   getStandard' sp' t (SplitP TopP:p)
    getStandard' (SplitP BottomP:sp') (HorizontalP _t b _) p =   getStandard' sp' b (SplitP BottomP:p)
    -- if no match get leftmost topmost
    getStandard' _ (VerticalP l _r _) p              =   getStandard' [] l (SplitP LeftP:p)
    getStandard' _ (HorizontalP t _b _) p            =   getStandard' [] t (SplitP TopP:p)

--
-- | Get a standard path.
--
getBestPathForId :: PaneMonad alpha => Text -> alpha PanePath
getBestPathForId id' = do
    p <- panePathForGroup id'
    getBestPanePath p <$> getLayout

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
    nbPtr <- liftIO $ unsafeManagedPtrCastPtr nb
    setPanePathFromNB $ Map.insert nbPtr pp (panePathFromNB st)
    func <- runInIO move'
    tl <- targetListNew Nothing
    targetListAddTextTargets tl 0
    widgetDragDestSet nb [DestDefaultsAll] Nothing [DragActionCopy, DragActionMove]
    widgetDragDestSetTargetList nb $ Just tl
    _ <- onWidgetDragDataReceived nb (dragFunc nb func)
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
        dragFunc nb func _cont _x _y data_ _id _timeStamp =
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
        terminalsFromGroup pp (name,layout')       =  terminalsWithPP (GroupP name : pp) layout'

findGroupPath :: Text -> PaneLayout -> Maybe PanePath
findGroupPath group layout' =
    let terminalPairs = terminalsWithPanePath layout'
    in case filter filterFunc terminalPairs of
        [] -> Nothing
        [(pp, _)] -> Just (pp ++ [GroupP group])
        _ -> error ("ViewFrame>>group name not unique: " ++ T.unpack group)
    where
        filterFunc (_, TerminalP groups _ _ _ _) =  group  `Set.member` Map.keysSet groups
        filterFunc _                             =  error "ViewFrame>>findGroupPath: impossible"

findDetachedPath :: Text -> PaneLayout -> Maybe PanePath
findDetachedPath id' layout' =
    let terminalPairs = terminalsWithPanePath layout'
    in case filter filterFunc terminalPairs of
        [] -> Nothing
        [(pp, _)] -> Just pp
        _ -> error ("ViewFrame>>window id not unique: " ++ T.unpack id')
    where
        filterFunc (_, TerminalP _ _ _ (Just lid) _) = lid == id'
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
otherSide p =
    case reverse p of
        (SplitP d:rest) -> Just . reverse $ SplitP (otherDirection d) : rest
        _               -> Nothing

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
layoutsFromPath (GroupP group:r) layout'@TerminalP {paneGroups = groups}
    | group `Map.member` groups
        = layout':layoutsFromPath r (groups Map.! group)
layoutsFromPath [] layout'                                      =   [layout']
layoutsFromPath (SplitP TopP:r) layout'@(HorizontalP t _b _)    =   layout':layoutsFromPath r t
layoutsFromPath (SplitP BottomP:r) layout'@(HorizontalP _t b _) =   layout':layoutsFromPath r b
layoutsFromPath (SplitP LeftP:r) layout'@(VerticalP l _ri _)    =   layout':layoutsFromPath r l
layoutsFromPath (SplitP RightP:r) layout'@(VerticalP _l ri _)   =   layout':layoutsFromPath r ri
layoutsFromPath pp l                                      = error
    $"inconsistent layout (layoutsFromPath) " ++ show pp ++ " " ++ show l

getWidgetNameList :: PanePath -> PaneLayout -> [Text]
getWidgetNameList path layout' = reverse $ nameList (reverse path) (reverse $ layoutsFromPath path layout')
    where
        nameList [] _ = reverse ["Leksah Main Window","topBox","root"]
        nameList (pe:_) (TerminalP{detachedId = Just id'}:_) = [panePathElementToWidgetName pe, id']
        nameList (pe:rpath) (_:rlayout) = panePathElementToWidgetName pe : nameList rpath rlayout
        nameList _ _ = error $ "inconsistent layout (getWidgetNameList) " ++ show path ++ " " ++ show layout'

getNotebookOrPaned :: PaneMonad alpha => PanePath -> (Widget -> IO beta) -> alpha beta
getNotebookOrPaned p cf = do
    layout' <- getLayout
    (widgetGet $ getWidgetNameList p layout') cf

--
-- | Get the notebook widget for the given pane path
--
getNotebook :: PaneMonad alpha => PanePath -> alpha  Notebook
getNotebook p = getNotebookOrPaned p (unsafeCastTo Notebook)

getNotebook' :: (HasCallStack, PaneMonad alpha) => Text -> PanePath -> alpha  Notebook
getNotebook' _str p = getNotebookOrPaned p (unsafeCastTo Notebook)


--
-- | Get the (gtk) Paned widget for a given path
--
getPaned :: PaneMonad alpha => PanePath -> alpha Paned
getPaned p = getNotebookOrPaned p $ unsafeCastTo Paned

--
-- | Get the path to the active pane
--
getActivePanePath :: PaneMonad alpha => alpha  (Maybe PanePath)
getActivePanePath =
    getActivePaneSt >>= \case
        (Nothing, _) -> return Nothing
        (Just (paneName',_),_) -> do
            (pp,_)  <- guiPropertiesFromName paneName'
            return (Just pp)

getActivePanePathOrStandard :: PaneMonad alpha => StandardPath -> alpha  PanePath
getActivePanePathOrStandard sp =
    getActivePanePath >>= \case
        Just app -> return app
        Nothing -> getBestPanePath sp <$> getLayoutSt

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
    paneMap'     <- getPaneMapSt
    setPaneMapSt (Map.map (\(pp,other) ->
        case stripPrefix fromPane pp of
            Just rest -> (toPane ++ rest,other)
            _         -> (pp,other)) paneMap')

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
    layout'         <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout'
        newLayout   =   TerminalP Map.empty Nothing 0 Nothing Nothing
        newTerm     =   case dir of
                            LeftP   -> VerticalP paneLayout newLayout 0
                            RightP  -> VerticalP newLayout paneLayout 0
                            TopP    -> HorizontalP paneLayout newLayout 0
                            BottomP -> HorizontalP newLayout paneLayout 0
    setLayoutSt     $   adjustLayout path layout' newTerm

--
-- | Changes the layout for a nest
--
adjustLayoutForNest :: PaneMonad alpha => Text -> PanePath -> alpha ()
adjustLayoutForNest group path = do
    layout'         <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout'
        newTerm     =   case paneLayout of
                            TerminalP {paneGroups = groups} -> paneLayout {
                                paneGroups = Map.insert group (TerminalP Map.empty Nothing 0 Nothing Nothing) groups}
                            _          -> error "Unexpected layout type in adjustLayoutForNest"
    setLayoutSt     $   adjustLayout path layout' newTerm

--
-- | Changes the layout for a detach
--
adjustLayoutForDetach :: PaneMonad alpha => Text -> PanePath -> alpha ()
adjustLayoutForDetach id' path = do
    layout'         <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout'
        newTerm     =   case paneLayout of
                            TerminalP {} -> paneLayout {detachedId = Just id'}
                            _            -> error "Unexpected layout type in adjustLayoutForDetach"
    setLayoutSt     $   adjustLayout path layout' newTerm

--
-- | Changes the layout for a reattach
--
adjustLayoutForReattach :: PaneMonad alpha => PanePath -> alpha ()
adjustLayoutForReattach path = do
    layout'         <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout'
        newTerm     =   case paneLayout of
                            TerminalP {} -> paneLayout {detachedId = Nothing, detachedSize = Nothing}
                            _ -> error "Unexpected layout type in adjustLayoutForReattach"
    setLayoutSt     $   adjustLayout path layout' newTerm

--
-- | Changes the layout for a collapse
--
adjustLayoutForCollapse :: PaneMonad alpha => PanePath -> alpha ()
adjustLayoutForCollapse oldPath  = do
    layout'         <-  getLayoutSt
    let pathLayout  =   layoutFromPath oldPath layout'
    setLayoutSt     $   adjustLayout (init oldPath) layout' pathLayout

--
-- | Changes the layout for a move
--
adjustLayoutForGroupMove :: PaneMonad alpha => PanePath -> PanePath -> Text -> alpha ()
adjustLayoutForGroupMove fromPath toPath group = do
    layout' <- getLayout
    let layoutToMove = layoutFromPath fromPath layout'
    let newLayout = removeGL fromPath layout'
    setLayoutSt (addGL layoutToMove (toPath ++ [GroupP group])  newLayout)

--
-- | Changes the layout for a remove
--
adjustLayoutForGroupRemove :: PaneMonad alpha => PanePath -> Text -> alpha ()
adjustLayoutForGroupRemove fromPath _group = do
    layout' <- getLayout
    setLayoutSt (removeGL fromPath layout')

--
-- | Remove group layout at a certain path
--
removeGL :: PanePath -> PaneLayout -> PaneLayout
removeGL [GroupP group] t@(TerminalP oldGroups _ _ _ _)
    | group `Map.member` oldGroups                        =  t{paneGroups = group `Map.delete` oldGroups}
removeGL (GroupP group:r)  old@TerminalP {paneGroups = groups}
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
addGL toAdd (GroupP group:r)  old@TerminalP {paneGroups = groups}
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
adjustLayout pp layout' replace    = adjust' pp layout'
    where
    adjust' [] _                                       = replace
    adjust' (GroupP group:r)  old@TerminalP {paneGroups = groups}
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
    windows' <- getWindowsSt
    widgets <- liftIO $ mapM toWidget windows'
    r <- liftIO $ chooseWidgetFromPath widgets strL
    liftIO (cf r)

getUIAction :: PaneMonad alpha => Text -> (Action -> IO a) -> alpha a
getUIAction str f = do
    uiManager' <- getUiManagerSt
    findAction <- uIManagerGetAction uiManager' str
    case findAction of
        Just act -> liftIO $ f act
        Nothing  -> error $"getUIAction can't find action " ++ T.unpack str

getThis :: PaneMonad delta =>  (FrameState delta -> alpha) -> delta alpha
getThis sel = sel <$> getFrameState
setThis :: PaneMonad delta =>  (FrameState delta -> alpha -> FrameState delta) -> alpha -> delta ()
setThis sel value = do
    st <- getFrameState
    trace ("!!! setFrameState " <> show (sel st value)) $ setFrameState (sel st value)

getWindowsSt :: PaneMonad alpha => alpha [Window]
getWindowsSt    = getThis windows
setWindowsSt :: PaneMonad alpha => [Window] -> alpha ()
setWindowsSt    = setThis (\st value -> st{windows = value})
getUiManagerSt :: PaneMonad alpha => alpha UIManager
getUiManagerSt  = getThis uiManager
getPanesSt :: PaneMonad alpha => alpha (Map PaneName (IDEPane alpha))
getPanesSt      = getThis panes
setPanesSt :: PaneMonad alpha => Map PaneName (IDEPane alpha) -> alpha ()
setPanesSt      = setThis (\st value -> st{panes = value})
getPaneMapSt :: PaneMonad alpha => alpha (Map PaneName (PanePath, [Connection]))
getPaneMapSt    = getThis paneMap
setPaneMapSt :: PaneMonad alpha => Map PaneName (PanePath, [Connection]) -> alpha ()
setPaneMapSt    = setThis (\st value -> st{paneMap = value})
getActivePaneSt :: PaneMonad alpha => alpha (Maybe (PaneName, [Connection]), [PaneName])
getActivePaneSt = getThis activePane
setActivePaneSt :: PaneMonad alpha => (Maybe (PaneName, [Connection]), [PaneName]) -> alpha ()
setActivePaneSt = setThis (\st value -> st{activePane = value})
getLayoutSt :: PaneMonad alpha => alpha PaneLayout
getLayoutSt     = getThis layout
setLayoutSt :: PaneMonad alpha => PaneLayout -> alpha ()
setLayoutSt     = setThis (\st value -> st{layout = value})
getPanePathFromNB :: PaneMonad alpha => alpha (Map (Ptr Notebook) PanePath)
getPanePathFromNB  = getThis panePathFromNB
setPanePathFromNB :: PaneMonad alpha => Map (Ptr Notebook) PanePath -> alpha ()
setPanePathFromNB  = setThis (\st value -> st{panePathFromNB = value})

getActivePane :: PaneMonad alpha => alpha (Maybe (PaneName, [Connection]), [PaneName])
getActivePane   = getActivePaneSt
setActivePane :: PaneMonad alpha => (Maybe (PaneName, [Connection]), [PaneName]) -> alpha ()
setActivePane   = setActivePaneSt
getUiManager :: PaneMonad alpha => alpha UIManager
getUiManager    = getUiManagerSt
getWindows :: PaneMonad alpha => alpha [Window]
getWindows      = getWindowsSt
getMainWindow :: PaneMonad alpha => alpha Window
getMainWindow   = head <$> getWindows
getLayout :: PaneMonad alpha => alpha PaneLayout
getLayout       = getLayoutSt
getMRUPanes :: PaneMonad alpha => alpha [PaneName]
getMRUPanes     =
    getActivePane >>= \case
        (Nothing, mru) -> return mru
        (Just (n, _), mru) -> return (n:mru)

