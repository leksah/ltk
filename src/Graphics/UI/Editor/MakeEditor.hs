{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
--group_Test
-- Module      :  Graphics.UI.Editor.MakeEditor
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for making editors out of descriptions
--
-----------------------------------------------------------------------------------

module Graphics.UI.Editor.MakeEditor (

    buildEditor
,   FieldDescription(..)
,   mkField
,   extractAndValidate
,   extract
,   mkEditor
,   parameters

,   flattenFieldDescription
,   getRealWidget
,   MkFieldDescription

) where

import Prelude ()
import Prelude.Compat
import Control.Monad (foldM, when)
import Data.List (unzip4, intersperse)
import Data.Text (Text)
import Data.Monoid (mconcat)

import Control.Event
import Graphics.UI.Utils
import Graphics.UI.Editor.Parameters
import Graphics.UI.Editor.Basics
--import Graphics.UI.Frame.ViewFrame
import Data.Maybe (fromMaybe, fromJust)
import GI.Gtk.Objects.Widget
       (toWidget, widgetSetName, widgetSetSizeRequest, Widget(..))
import GI.Gtk.Objects.Notebook
       (setNotebookEnablePopup, notebookSetCurrentPage,
        notebookAppendPage, notebookSetScrollable, notebookSetShowTabs,
        notebookSetTabPos, notebookNew, Notebook(..))
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowNew)
import GI.Gtk.Objects.TreeView
       (treeViewSetHeadersVisible, treeViewAppendColumn,
        treeViewGetSelection, treeViewNewWithModel)
import GI.Gtk.Objects.TreeSelection
       (onTreeSelectionChanged,
        treeSelectionSelectPath, treeSelectionSetMode)
import GI.Gtk.Objects.CellRendererText
       (setCellRendererTextText, cellRendererTextNew)
import GI.Gtk.Objects.TreeViewColumn (treeViewColumnNew)
import GI.Gtk.Interfaces.CellLayout
       (cellLayoutSetCellDataFunc, cellLayoutPackStart)
import GI.Gtk.Objects.Container
       (toContainer, Container(..), containerAdd)
import GI.Gtk.Objects.Frame
       (frameSetLabel, frameSetShadowType, frameNew)
import GI.Gtk.Objects.Bin (Bin(..), binGetChild)
import GI.Gtk.Enums
       (Orientation(..), Align(..), SelectionMode(..), PolicyType(..),
        PositionType(..))
import Data.GI.Base.ManagedPtr (castTo)
import Data.GI.Gtk.ModelView.CustomStore (customStoreGetRow)
import GI.Gtk.Objects.Adjustment (Adjustment)
import Data.GI.Gtk.ModelView.SeqStore (seqStoreNew)
import GI.Gtk.Objects.Label (labelNew)
import Control.Exception (catch)
import Data.GI.Base.BasicTypes (UnexpectedNullPointerReturn(..))
import GI.Gtk.Structs.TreePath (treePathNewFirst)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathGetIndices')
import Control.Monad.IO.Class (MonadIO(..), MonadIO)
import GI.Gtk
       (orientableSetOrientation, widgetSetVexpand,
        widgetSetHexpand, widgetSetMarginEnd, widgetSetMarginStart,
        widgetSetMarginBottom, widgetSetMarginTop,
        widgetSetValign, widgetSetHalign, gridNew)
import Data.Foldable (forM_)

--
-- | A constructor type for a field desciption
--
type MkFieldDescription alpha beta =
    Parameters ->
    Getter alpha beta ->
    Setter alpha beta ->
    Editor beta ->
    FieldDescription alpha

--
-- | A type to describe a field of a record, which can be edited
-- | alpha is the type of the individual field of the record
data FieldDescription alpha =  FD Parameters (alpha -> IO (Widget, Injector alpha ,
                                    alpha -> Extractor alpha , Notifier))
    | VFD Parameters [FieldDescription alpha]
    | HFD Parameters [FieldDescription alpha]
    | NFD [(Text,FieldDescription alpha)]

parameters :: FieldDescription alpha -> Parameters
parameters (FD p _) = p
parameters (VFD p _) = p
parameters (HFD p _) = p
parameters (NFD _) = emptyParams

--
-- | Construct a new notebook
--
newNotebook :: MonadIO m => m Notebook
newNotebook = do
    nb <- notebookNew
    notebookSetTabPos nb PositionTypeTop
    notebookSetShowTabs nb True
    notebookSetScrollable nb True
    setNotebookEnablePopup nb True
    return nb

buildEditor :: (Applicative m, MonadIO m) => FieldDescription alpha -> alpha -> m (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
buildEditor (FD _paras editorf) v = liftIO $ editorf v
buildEditor (HFD _paras descrs) v = buildBoxEditor descrs OrientationHorizontal v
buildEditor (VFD _paras descrs) v = buildBoxEditor descrs OrientationVertical v
buildEditor (NFD pairList)      v = do
    nb <- newNotebook
    notebookSetShowTabs nb False
    resList <- mapM ((`buildEditor` v) . snd) pairList
    let (widgets, setInjs, getExts, notifiers) = unzip4 resList
    notifier <- emptyNotifier
    mapM_ (\ (labelString, widget) -> do
        sw <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
        containerAdd sw widget
        scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic
        notebookAppendPage nb sw . Just =<< labelNew (Just labelString))
         (zip (map fst pairList) widgets)
    seqStore   <- seqStoreNew (map fst pairList)
    listView    <- treeViewNewWithModel seqStore
    widgetSetSizeRequest listView 100 (-1)
    sel         <- treeViewGetSelection listView
    treeSelectionSetMode sel SelectionModeSingle
    renderer    <- cellRendererTextNew
    col         <- treeViewColumnNew
    _ <- treeViewAppendColumn listView col
    cellLayoutPackStart col renderer True
    cellLayoutSetCellDataFunc col renderer . Just $ \_c _r _m i -> do
        row <- customStoreGetRow seqStore i
        setCellRendererTextText renderer row
    treeViewSetHeadersVisible listView False
    treeSelectionSelectPath sel =<< treePathNewFirst
    notebookSetCurrentPage nb 0
    _ <- onTreeSelectionChanged sel $ do
        selections <- treeSelectionGetSelectedRows' sel >>= mapM treePathGetIndices'
        case selections of
            [[i]] -> notebookSetCurrentPage nb i
            _ -> return ()

    grid        <- gridNew
    sw          <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
    containerAdd sw listView
    scrolledWindowSetPolicy sw PolicyTypeNever PolicyTypeAutomatic
    widgetSetHalign sw AlignStart
    widgetSetVexpand sw True
    containerAdd grid sw
    widgetSetHexpand nb True
    widgetSetVexpand nb True
    containerAdd grid nb
    let newInj v' = mapM_ (\ setInj -> setInj v') setInjs
    let newExt v' = extract v' getExts
    mapM_ (propagateEvent notifier notifiers) allGUIEvents
    widget <- toWidget grid
    return (widget, newInj, newExt, notifier)

buildBoxEditor :: (Applicative m, MonadIO m) => [FieldDescription alpha] -> Orientation -> alpha
    -> m (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
buildBoxEditor descrs orientation v = do
    resList <- mapM (`buildEditor` v) descrs
    notifier <- emptyNotifier
    let (widgets, setInjs, getExts, notifiers) = unzip4 resList
    grid <- gridNew
    orientableSetOrientation grid orientation
    let newInj v' = mapM_ (\ setInj -> setInj v') setInjs
    let fieldNames = map (fromMaybe "Unnamed" . getParameterPrim paraName . parameters) descrs
    let packParas = map (getParameter paraPack . parameters) descrs
    mapM_ (propagateEvent notifier notifiers) allGUIEvents
    let newExt v' = extractAndValidate v' getExts fieldNames notifier
    forM_ (zip widgets packParas) $ \(w, p) -> do
        case p of
            PackRepel   -> setPrimaryAlign grid w AlignEnd >> setPrimaryExpand grid w True
            PackNatural -> setPrimaryAlign grid w AlignStart
            PackGrow    -> setPrimaryExpand grid w True
        setSecondaryExpand grid w True
        containerAdd grid w
    hbWidget <- liftIO $ toWidget grid
    return (hbWidget, newInj, newExt, notifier)


flattenFieldDescription :: FieldDescription alpha -> [FieldDescription alpha]
flattenFieldDescription (VFD _paras descrs) =   concatMap flattenFieldDescription descrs
flattenFieldDescription (HFD _paras descrs) =   concatMap flattenFieldDescription descrs
flattenFieldDescription (NFD descrp)        =   concatMap (flattenFieldDescription.snd) descrp
flattenFieldDescription fd                  =   [fd]

-- ------------------------------------------------------------
-- * Implementation of editing
-- ------------------------------------------------------------

--
-- | Function to construct a field description
--
mkField :: Eq beta => MkFieldDescription alpha beta
mkField params getter setter editor =
    FD params
        (\ dat -> do
            noti <- emptyNotifier
            (widget,inj,ext) <- editor params noti
            let pext a = ext >>= \case
                                Just b -> return (Just (setter b a))
                                Nothing -> return Nothing
            inj (getter dat)
            return (widget,
                    inj . getter,
                    pext,
                    noti))

-- | Function to construct an editor
--
mkEditor :: (Container -> Injector alpha) -> Extractor alpha -> Editor alpha
mkEditor injectorC extractor params _notifier = do
    frame   <-  frameNew Nothing
    widgetSetHalign frame $ getParameter paraHAlign params
    widgetSetValign frame $ getParameter paraVAlign params
    let (top, bottom, start, end) = getParameter paraMargin params
    widgetSetMarginTop frame top
    widgetSetMarginBottom frame bottom
    widgetSetMarginStart frame start
    widgetSetMarginEnd frame end
    frameSetShadowType frame (getParameter paraShadow params)
    case getParameter paraName params of
        "" -> return ()
        str -> when (getParameter paraShowLabel params) $
                  frameSetLabel frame (Just str)
    let (x,y) = getParameter paraMinSize params
    widgetSetSizeRequest frame x y
    let name  =  getParameter paraName params
    widgetSetName frame name
    build <- injectorC <$> toContainer frame
    w <- toWidget frame
    return (w, build, extractor)

-- | Convenience method to validate and extract fields
--
extractAndValidate :: MonadIO m => alpha -> [alpha -> Extractor alpha] -> [Text] -> Notifier -> m (Maybe alpha)
extractAndValidate val getExts fieldNames notifier = do
    (newVal,errors) <- foldM (\ (val',errs) (ext,fn) -> do
        extVal <- liftIO $ ext val'
        case extVal of
            Just nval -> return (nval,errs)
            Nothing -> return (val', (" " <> fn) : errs))
                (val,[]) (zip getExts fieldNames)
    if null errors
        then return (Just newVal)
        else do
            _ <- liftIO $ triggerEvent notifier GUIEvent {
                    selector = ValidationError,
                    eventText = mconcat (intersperse ", " errors),
                    gtkReturn = True}
            return Nothing

extract :: MonadIO m => alpha -> [alpha -> Extractor alpha] -> m (Maybe alpha)
extract val  =
    foldM (\ mbVal ext ->
        case mbVal of
            Nothing -> return Nothing
            Just val' -> liftIO $ ext val')
            (Just val)

-- | get through outerAlignment, frame, innerAlignment
getRealWidget :: MonadIO m => Widget -> m (Maybe Widget)
getRealWidget w = liftIO $ (
    castTo Bin w >>= \case
        Nothing -> return Nothing
        Just b  ->
            (fromJust <$> binGetChild b) >>= castTo Bin >>= \case
                Nothing -> return Nothing
                Just f  ->
                    (fromJust <$> binGetChild f) >>= castTo Bin >>= \case
                        Nothing -> return Nothing
                        Just ia -> binGetChild ia
  ) `catch` (\UnexpectedNullPointerReturn {} -> return Nothing)




