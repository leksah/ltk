{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Monad
import Data.List (unzip4, intersperse)
import Data.Text (Text)
import Data.Monoid ((<>), mconcat)

import Control.Event
import Graphics.UI.Editor.Parameters
import Graphics.UI.Editor.Basics
--import Graphics.UI.Frame.ViewFrame
import Data.Maybe (fromMaybe, isNothing)
import Data.IORef (newIORef)
import GI.Gtk.Objects.Widget
       (toWidget, widgetSetName, widgetSetSizeRequest, Widget(..))
import GI.Gtk.Objects.Notebook
       (setNotebookEnablePopup, notebookSetCurrentPage,
        notebookAppendPage, notebookSetScrollable, notebookSetShowTabs,
        notebookSetTabPos, notebookNew, Notebook(..))
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy, scrolledWindowAddWithViewport,
        scrolledWindowNew)
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
import Data.GI.Base.Attributes (AttrOp(..))
import Data.GI.Base.Signals (on)
import GI.Gtk.Objects.HBox (hBoxNew)
import GI.Gtk.Objects.Container
       (toContainer, Container(..), containerAdd)
import GI.Gtk.Objects.Box (Box(..), boxPackEnd, boxPackStart)
import GI.Gtk.Objects.VBox (vBoxNew)
import GI.Gtk.Objects.Alignment (alignmentSetPadding, alignmentNew)
import GI.Gtk.Objects.Frame
       (frameSetLabel, frameSetShadowType, frameNew)
import GI.Gtk.Objects.Bin (Bin(..), binGetChild)
import GI.Gtk.Enums
       (SelectionMode(..), PolicyType(..), PositionType(..))
import Data.GI.Base.ManagedPtr (castTo, unsafeCastTo)
import Data.GI.Gtk.ModelView.CustomStore (customStoreGetRow)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import Data.GI.Gtk.ModelView.SeqStore (SeqStore(..), seqStoreNew)
import GI.Gtk.Objects.Label (labelNew)
import Control.Exception (catch)
import Data.GI.Base.BasicTypes (UnexpectedNullPointerReturn(..))
import GI.Gtk.Structs.TreePath (treePathNewFirst)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathGetIndices')
import Control.Monad.IO.Class (MonadIO(..), MonadIO)

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

buildEditor :: MonadIO m => FieldDescription alpha -> alpha -> m (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
buildEditor (FD paras editorf) v  =  liftIO $ editorf v
buildEditor (HFD paras descrs) v =   buildBoxEditor descrs Horizontal v
buildEditor (VFD paras descrs) v =   buildBoxEditor descrs Vertical v
buildEditor (NFD pairList)     v =   do
    nb <- newNotebook
    notebookSetShowTabs nb False
    resList <- mapM ((`buildEditor` v) . snd) pairList
    let (widgets, setInjs, getExts, notifiers) = unzip4 resList
    notifier <- emptyNotifier
    mapM_ (\ (labelString, widget) -> do
        sw <- scrolledWindowNew noAdjustment noAdjustment
        scrolledWindowAddWithViewport sw widget
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
    treeViewAppendColumn listView col
    cellLayoutPackStart col renderer True
    cellLayoutSetCellDataFunc col renderer . Just $ \c r m i -> do
        row <- customStoreGetRow seqStore i
        setCellRendererTextText renderer row
    treeViewSetHeadersVisible listView False
    treeSelectionSelectPath sel =<< treePathNewFirst
    notebookSetCurrentPage nb 0
    onTreeSelectionChanged sel $ do
        selections <- treeSelectionGetSelectedRows' sel >>= mapM treePathGetIndices'
        case selections of
            [[i]] -> notebookSetCurrentPage nb i
            _ -> return ()

    hb          <- hBoxNew False 0
    sw          <- scrolledWindowNew noAdjustment noAdjustment
    containerAdd sw listView
    scrolledWindowSetPolicy sw PolicyTypeNever PolicyTypeAutomatic
    boxPackStart' hb sw PackNatural 0
    boxPackEnd' hb nb PackGrow 7
    let newInj v = mapM_ (\ setInj -> setInj v) setInjs
    let newExt v = extract v getExts
    mapM_ (propagateEvent notifier notifiers) allGUIEvents
    widget <- liftIO $ toWidget hb
    return (widget, newInj, newExt, notifier)

buildBoxEditor :: MonadIO m => [FieldDescription alpha] -> Direction -> alpha
    -> m (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
buildBoxEditor descrs dir v = do
    resList <- mapM (`buildEditor` v) descrs
    notifier <- emptyNotifier
    let (widgets, setInjs, getExts, notifiers) = unzip4 resList
    hb <- case dir of
            Horizontal -> hBoxNew False 0 >>= liftIO . unsafeCastTo Box
            Vertical   -> vBoxNew False 0 >>= liftIO . unsafeCastTo Box
    let newInj v = mapM_ (\ setInj -> setInj v) setInjs
    let fieldNames = map (fromMaybe "Unnamed" . getParameterPrim paraName . parameters) descrs
    let packParas = map (getParameter paraPack . parameters) descrs
    mapM_ (propagateEvent notifier notifiers) allGUIEvents
    let newExt v = extractAndValidate v getExts fieldNames notifier
    mapM_ (\ (w,p) -> boxPackStart' hb w p 0) $ zip widgets packParas
    hbWidget <- liftIO $ toWidget hb
    return (hbWidget, newInj, newExt, notifier)


flattenFieldDescription :: FieldDescription alpha -> [FieldDescription alpha]
flattenFieldDescription (VFD paras descrs)  =   concatMap flattenFieldDescription descrs
flattenFieldDescription (HFD paras descrs)  =   concatMap flattenFieldDescription descrs
flattenFieldDescription (NFD descrp)        =   concatMap (flattenFieldDescription.snd) descrp
flattenFieldDescription fd                  =   [fd]

-- ------------------------------------------------------------
-- * Implementation of editing
-- ------------------------------------------------------------

--
-- | Function to construct a field description
--
mkField :: Eq beta => MkFieldDescription alpha beta
mkField parameters getter setter editor =
    FD parameters
        (\ dat -> do
            noti <- emptyNotifier
            (widget,inj,ext) <- editor parameters noti
            let pext a = do
                            b <- ext
                            case b of
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
mkEditor injectorC extractor parameters notifier = do
    let (xalign, yalign, xscale, yscale) = getParameter paraOuterAlignment parameters
    outerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter paraOuterPadding parameters
    alignmentSetPadding outerAlig paddingTop paddingBottom paddingLeft paddingRight
    frame   <-  frameNew Nothing
    frameSetShadowType frame (getParameter paraShadow parameters)
    case getParameter paraName parameters of
        "" -> return ()
        str -> when (getParameter paraShowLabel parameters) $
                  frameSetLabel frame (Just str)

    containerAdd outerAlig frame
    let (xalign, yalign, xscale, yscale) =  getParameter paraInnerAlignment parameters
    innerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter paraInnerPadding parameters
    alignmentSetPadding innerAlig paddingTop paddingBottom paddingLeft paddingRight
    containerAdd frame innerAlig
    let (x,y) = getParameter paraMinSize parameters
    widgetSetSizeRequest outerAlig x y
    let name  =  getParameter paraName parameters
    widgetSetName outerAlig name
    build <- injectorC <$> toContainer innerAlig
    w <- toWidget outerAlig
    return (w, build, extractor)

-- | Convenience method to validate and extract fields
--
extractAndValidate :: MonadIO m => alpha -> [alpha -> Extractor alpha] -> [Text] -> Notifier -> m (Maybe alpha)
extractAndValidate val getExts fieldNames notifier = do
    (newVal,errors) <- foldM (\ (val,errs) (ext,fn) -> do
        extVal <- liftIO $ ext val
        case extVal of
            Just nval -> return (nval,errs)
            Nothing -> return (val, (" " <> fn) : errs))
                (val,[]) (zip getExts fieldNames)
    if null errors
        then return (Just newVal)
        else do
            liftIO $ triggerEvent notifier GUIEvent {
                    selector = ValidationError,
                    eventText = mconcat (intersperse ", " errors),
                    gtkReturn = True}
            return Nothing

extract :: MonadIO m => alpha -> [alpha -> Extractor alpha] -> m (Maybe alpha)
extract val  =
    foldM (\ mbVal ext ->
        case mbVal of
            Nothing -> return Nothing
            Just val -> liftIO $ ext val)
            (Just val)

-- | get through outerAlignment, frame, innerAlignment
getRealWidget :: MonadIO m => Widget -> m (Maybe Widget)
getRealWidget w = liftIO $ (
    castTo Bin w >>= \case
        Nothing -> return Nothing
        Just b  ->
            binGetChild b >>= castTo Bin >>= \case
                Nothing -> return Nothing
                Just f  ->
                    binGetChild f >>= castTo Bin >>= \case
                        Nothing -> return Nothing
                        Just ia -> Just <$> binGetChild ia
  ) `catch` (\UnexpectedNullPointerReturn {} -> return Nothing)




