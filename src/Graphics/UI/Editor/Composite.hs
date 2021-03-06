{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.Composite
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for making composite editors
--
-----------------------------------------------------------------------------------

module Graphics.UI.Editor.Composite (
    maybeEditor
,   disableEditor
,   pairEditor
,   tupel3Editor
,   splitEditor
,   eitherOrEditor
,   multisetEditor
,   ColumnDescr(..)

,   filesEditor
,   textsEditor

,   versionEditor
,   versionRangeEditor
,   dependencyEditor
,   dependenciesEditor
) where

import Prelude ()
import Prelude.Compat
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Set as S (singleton)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)

import Data.Default (Default(..))
import Control.Event
import Graphics.UI.Utils
import Graphics.UI.Editor.Parameters
import Graphics.UI.Editor.Basics
import Graphics.UI.Editor.MakeEditor
import Graphics.UI.Editor.Simple
import Data.List (sortBy, nub, sort, elemIndex)
#if MIN_VERSION_Cabal (3,0,0)
import Distribution.Types.LibraryName (LibraryName(..))
#else
import Distribution.Types.UnqualComponentName (UnqualComponentName)
#endif
import Distribution.Simple
    (orEarlierVersion,
     orLaterVersion,
     VersionRange,
     PackageName,
     Dependency(..),
     unPackageName,
     mkPackageName,
     PackageIdentifier(..))
import Distribution.Text (simpleParse, display)
import Distribution.Package (pkgName)
import GI.Gtk
       (gridSetRowSpacing, gridSetColumnSpacing, orientableSetOrientation,
        buttonBoxNew, panedNew, gridNew, setCellRendererTextText,
        TreeViewColumn, Adjustment, FileChooserAction,
        treeModelGetPath, treeSelectionGetSelected, treeViewScrollToCell,
        treeSelectionSelectPath, onButtonClicked, onTreeSelectionChanged,
        treeViewSetHeadersVisible, cellLayoutPackStart,
        cellRendererTextNew, treeViewAppendColumn,
        treeViewColumnSetResizable, treeViewColumnSetTitle,
        treeViewColumnNew, treeSelectionSetMode, treeViewGetSelection,
        scrolledWindowSetMinContentHeight, scrolledWindowSetPolicy,
        scrolledWindowNew, widgetSetSizeRequest, treeViewNewWithModel,
        afterTreeModelRowDeleted, afterTreeModelRowInserted,
        buttonNewWithLabel, CellRendererText, containerRemove,
        widgetSetSensitive, containerGetChildren, widgetHide,
        widgetShowAll, panedPack2, panedPack1, containerAdd, Widget(..))
import GI.Gtk.Enums
       (Orientation(..), Align(..), ShadowType(..),
        SelectionMode(..), PolicyType(..))
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreAppend, seqStoreClear, seqStoreNew, seqStoreGetValue,
        seqStoreRemove, seqStoreToList)
import Data.GI.Gtk.ModelView.Types
       (treePathNewFromIndices', equalManagedPtr)
import GI.Gtk.Structs.TreePath (treePathGetIndices)
import Distribution.Version
       (Version, majorBoundVersion,
        withinVersion, intersectVersionRanges, unionVersionRanges,
        earlierVersion, laterVersion, thisVersion, anyVersion,
        cataVersionRange, normaliseVersionRange, VersionRangeF(..))

#if MIN_VERSION_Cabal (3,0,0)
viewDependency (Dependency a b c) = (a, b, c)
mkDependency a b c = Dependency a b c
#else
data LibraryName = LMainLibName | LSubLibName UnqualComponentName
viewDependency (Dependency a b) = (a, b, S.singleton LMainLibName)
mkDependency a b _ = Dependency a b
#endif

--
-- | An editor which composes two subeditors
--
pairEditor :: (Editor alpha, Parameters) -> (Editor beta, Parameters) -> Editor (alpha,beta)
pairEditor (fstEd,fstPara) (sndEd,sndPara) params notifier = do
    coreRef <- newIORef Nothing
    noti1   <- emptyNotifier
    noti2   <- emptyNotifier
    mapM_ (propagateEvent notifier [noti1,noti2]) allGUIEvents
    fst'@(fstFrame,inj1,_ext1) <- fstEd fstPara noti1
    snd'@(sndFrame,inj2,_ext2) <- sndEd sndPara noti2
    mkEditor
        (\widget (v1,v2) -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    grid <- gridNew
                    orientableSetOrientation grid $ getParameter paraOrientation params
                    gridSetColumnSpacing grid 1
                    gridSetRowSpacing grid 1
                    setPrimaryExpand grid fstFrame True
                    containerAdd grid fstFrame
                    setPrimaryExpand grid sndFrame True
                    containerAdd grid sndFrame
                    containerAdd widget grid
                    inj1 v1
                    inj2 v2
                    writeIORef coreRef (Just (fst',snd'))
                Just ((_,inj1',_),(_,inj2',_)) -> do
                    inj1' v1
                    inj2' v2)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2)) -> do
                    r1 <- ext1
                    r2 <- ext2
                    if isJust r1 && isJust r2
                        then return (Just (fromJust r1,fromJust r2))
                        else return Nothing)
        params
        notifier

tupel3Editor :: (Editor alpha, Parameters)
    -> (Editor beta, Parameters)
    -> (Editor gamma, Parameters)
    -> Editor (alpha,beta,gamma)
tupel3Editor p1 p2 p3 params notifier = do
    coreRef <- newIORef Nothing
    noti1   <- emptyNotifier
    noti2   <- emptyNotifier
    noti3   <- emptyNotifier
    mapM_ (propagateEvent notifier [noti1,noti2,noti3]) (Clicked : allGUIEvents)
    r1@(frame1,inj1,_ext1) <- fst p1 (snd p1) noti1
    r2@(frame2,inj2,_ext2) <- fst p2 (snd p2) noti2
    r3@(frame3,inj3,_ext3) <- fst p3 (snd p3) noti3
    mkEditor
        (\widget (v1,v2,v3) -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    grid <- gridNew
                    orientableSetOrientation grid $ getParameter paraOrientation params
                    setPrimaryExpand grid frame1 True
                    containerAdd grid frame1
                    setPrimaryExpand grid frame2 True
                    containerAdd grid frame2
                    setPrimaryExpand grid frame3 True
                    containerAdd grid frame3
                    containerAdd widget grid
                    inj1 v1
                    inj2 v2
                    inj3 v3
                    writeIORef coreRef (Just (r1,r2,r3))
                Just ((_,inj1',_),(_,inj2',_),(_,inj3',_)) -> do
                    inj1' v1
                    inj2' v2
                    inj3' v3)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2),(_,_,ext3)) -> do
                    r1' <- ext1
                    r2' <- ext2
                    r3' <- ext3
                    if isJust r1' && isJust r2' && isJust r3'
                        then return (Just (fromJust r1',fromJust r2', fromJust r3'))
                        else return Nothing)
        params
        notifier

--
-- | Like a pair editor, but with a moveable split
--
splitEditor :: (Editor alpha, Parameters) -> (Editor beta, Parameters) -> Editor (alpha,beta)
splitEditor (fstEd,fstPara) (sndEd,sndPara) params notifier = do
    coreRef <- newIORef Nothing
    noti1   <- emptyNotifier
    noti2   <- emptyNotifier
    mapM_ (propagateEvent notifier [noti1,noti2]) allGUIEvents
    fst'@(fstFrame,inj1,_ext1) <- fstEd fstPara noti1
    snd'@(sndFrame,inj2,_ext2) <- sndEd sndPara noti2
    mkEditor
        (\widget (v1,v2) -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    paned <- panedNew $ case getParameter paraOrientation params of
                        OrientationHorizontal -> OrientationVertical
                        OrientationVertical   -> OrientationHorizontal
                        _ -> error "Invalid Orientation"
                    panedPack1 paned fstFrame True True
                    panedPack2 paned sndFrame True True
                    containerAdd widget paned
                    inj1 v1
                    inj2 v2
                    writeIORef coreRef (Just (fst', snd'))
                Just ((_,inj1',_),(_,inj2',_)) -> do
                    inj1' v1
                    inj2' v2)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2)) -> do
                    r1 <- ext1
                    r2 <- ext2
                    if isJust r1 && isJust r2
                        then return (Just (fromJust r1,fromJust r2))
                        else return Nothing)
        params
        notifier

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- or deselected (if the positive Argument is False)
--
maybeEditor :: beta -> (Editor beta, Parameters) -> Bool -> Text -> Editor (Maybe beta)
maybeEditor initialValue (childEdit, childParams) positive boolLabel params notifier = do
    coreRef      <- newIORef Nothing
    childRef     <- newIORef Nothing
    notifierBool <- emptyNotifier
    cNoti        <- emptyNotifier
    mkEditor
        (\widget mbVal -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    grid <- gridNew
                    orientableSetOrientation grid $ getParameter paraOrientation params
                    gridSetColumnSpacing grid 1
                    gridSetRowSpacing grid 1
                    be@(boolFrame,inj1,_ext1) <- boolEditor
                        (paraName <<<- ParaName boolLabel $ emptyParams)
                        notifierBool
                    setPrimaryAlign grid boolFrame AlignStart
                    containerAdd grid boolFrame
                    containerAdd widget grid
                    _ <- registerEvent notifierBool Clicked (onClickedHandler widget coreRef childRef cNoti)
                    propagateEvent notifier [notifierBool] MayHaveChanged
                    case mbVal of
                        Nothing -> inj1 (not positive)
                        Just val -> do
                            (childWidget,inj2,_ext2) <- getChildEditor childRef childEdit childParams cNoti
                            setPrimaryExpand grid childWidget True
                            containerAdd grid childWidget
                            widgetShowAll childWidget
                            inj1 positive
                            inj2 val
                    writeIORef coreRef (Just (be,grid))
                Just ((_boolFrame,inj1,_extt),grid) -> do
                    hasChild <- hasChildEditor childRef
                    case mbVal of
                        Nothing ->
                            if hasChild
                                then do
                                    (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                    inj1 (not positive)
                                    widgetHide childWidget
                                else inj1 (not positive)
                        Just val ->
                            if hasChild
                                then do
                                    inj1 positive
                                    (childWidget,inj2,_) <- getChildEditor childRef childEdit childParams cNoti
                                    widgetShowAll childWidget
                                    inj2 val
                                else do
                                    inj1 positive
                                    (childWidget,inj2,_) <- getChildEditor childRef childEdit childParams cNoti
                                    setPrimaryExpand grid childWidget True
                                    containerAdd grid childWidget
                                    widgetShowAll childWidget
                                    inj2 val)
        (do
            core <- readIORef coreRef
            case core of
                Nothing  -> return Nothing
                Just ((_boolFrame,_inj1,ext1),_) -> do
                    bool <- ext1
                    case bool of
                        Nothing -> return Nothing
                        Just bv | bv == positive -> do
                            (_,_,ext2) <- getChildEditor childRef childEdit childParams cNoti
                            ext2 >>= \case
                                Nothing -> return Nothing
                                Just value -> return (Just (Just value))
                        _ -> return (Just Nothing))
        params
        notifier
    where
    onClickedHandler _widget coreRef childRef cNoti event = do
        core <- readIORef coreRef
        case core of
            Nothing  -> error "Impossible"
            Just ((_boolFrame,_inj1,ext1),grid) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->
                        if bool /= positive
                            then do
                                hasChild <- hasChildEditor childRef
                                when hasChild $ do
                                    (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                    widgetHide childWidget
                            else do
                                _hasChild <- hasChildEditor childRef
                                (childWidget,inj2,_ext2) <- getChildEditor childRef childEdit childParams cNoti
                                children <- containerGetChildren grid
                                unless (any (equalManagedPtr childWidget) children) $ do
                                    setPrimaryAlign grid childWidget AlignStart
                                    containerAdd grid childWidget
                                inj2 initialValue
                                widgetShowAll childWidget
                    Nothing -> return ()
                return (event {gtkReturn=True})
    getChildEditor childRef childEditor _childParams cNoti =  do
        mb <- readIORef childRef
        case mb of
            Just editor -> return editor
            Nothing -> do
                editor@(_,_,_) <- childEditor childParams cNoti
                mapM_ (propagateEvent notifier [cNoti]) allGUIEvents
                writeIORef childRef (Just editor)
                return editor
    hasChildEditor childRef =  do
        mb <- readIORef childRef
        return (isJust mb)


--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- or grayed out (if the positive Argument is False)
--
disableEditor :: beta -> (Editor beta, Parameters) -> Bool -> Text -> Editor (Bool,beta)
disableEditor defaultValue (childEdit, childParams) positive boolLabel params notifier = do
    coreRef      <- newIORef Nothing
    childRef     <- newIORef Nothing
    notifierBool <- emptyNotifier
    cNoti        <- emptyNotifier
    mkEditor
        (\widget mbVal -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    grid <- gridNew
                    orientableSetOrientation grid $ getParameter paraOrientation params
                    be@(boolFrame,inj1,_ext1) <- boolEditor
                        (paraName <<<- ParaName boolLabel $ emptyParams)
                        notifierBool
                    setPrimaryAlign grid boolFrame AlignStart
                    containerAdd grid boolFrame
                    containerAdd widget grid
                    _ <- registerEvent notifierBool Clicked
                        (onClickedHandler widget coreRef childRef cNoti)
                    propagateEvent notifier [notifierBool] MayHaveChanged
                    case mbVal of
                        (False,val) -> do
                            (childWidget,inj2,_ext2) <- getChildEditor childRef childEdit childParams cNoti
                            setPrimaryExpand grid childWidget True
                            containerAdd grid childWidget
                            widgetShowAll childWidget
                            inj1 ( not positive)
                            inj2 val
                            widgetSetSensitive childWidget False
                        (True,val) -> do
                            (childWidget,inj2,_ext2) <- getChildEditor childRef childEdit childParams cNoti
                            setPrimaryExpand grid childWidget True
                            containerAdd grid childWidget
                            widgetShowAll childWidget
                            inj1 positive
                            inj2 val
                            widgetSetSensitive childWidget True
                    writeIORef coreRef (Just (be,grid))
                Just ((_boolFrame,inj1,_extt),grid) -> do
                    hasChild <- hasChildEditor childRef
                    case mbVal of
                        (False,_val) ->
                            if hasChild
                                then do
                                    (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                    inj1 (not positive)
                                    widgetSetSensitive childWidget False
                                else inj1 (not positive)
                        (True,val) ->
                            if hasChild
                                then do
                                    inj1 positive
                                    (childWidget,inj2,_) <- getChildEditor childRef childEdit childParams cNoti
                                    inj2 val
                                    widgetSetSensitive childWidget True
                                else do
                                    inj1 positive
                                    (childWidget,inj2,_) <- getChildEditor childRef childEdit childParams cNoti
                                    setPrimaryExpand grid childWidget True
                                    containerAdd grid childWidget
                                    widgetSetSensitive childWidget True
                                    inj2 val)
        (do
            core <- readIORef coreRef
            case core of
                Nothing  -> return Nothing
                Just ((_boolFrame,_inj1,ext1),_) -> do
                    bool <- ext1
                    case bool of
                        Nothing -> return Nothing
                        Just bv | bv == positive -> do
                            (_,_,ext2) <- getChildEditor childRef childEdit childParams cNoti
                            ext2 >>= \case
                                Nothing -> return Nothing
                                Just value -> return (Just (True, value))
                        _ -> do
                            (_,_,ext2) <- getChildEditor childRef childEdit childParams cNoti
                            ext2 >>= \case
                                Nothing -> return Nothing
                                Just value -> return (Just (False, value)))
        params
        notifier
    where
    onClickedHandler _widget coreRef childRef cNoti event = do
        core <- readIORef coreRef
        case core of
            Nothing  -> error "Impossible"
            Just ((_boolFrame,_inj1,ext1),grid) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->
                        if bool /= positive
                            then do

                                hasChild <- hasChildEditor childRef
                                when hasChild $ do
                                    (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                    widgetSetSensitive childWidget False
                            else do
                                hasChild <- hasChildEditor childRef
                                if hasChild
                                    then do
                                        (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                        widgetSetSensitive childWidget True
                                    else do
                                        (childWidget,inj2,_) <- getChildEditor childRef childEdit childParams cNoti
                                        setPrimaryAlign grid childWidget AlignStart
                                        containerAdd grid childWidget
                                        inj2 defaultValue
                                        widgetSetSensitive childWidget True
                    Nothing -> return ()
                return (event {gtkReturn=True})
    getChildEditor childRef childEditor childParams' cNoti =  do
        mb <- readIORef childRef
        case mb of
            Just editor -> return editor
            Nothing -> do
                editor@(_,_,_) <- childEditor childParams' cNoti
                mapM_ (propagateEvent notifier [cNoti]) allGUIEvents
                writeIORef childRef (Just editor)
                return editor
    hasChildEditor childRef =  do
        mb <- readIORef childRef
        return (isJust mb)

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- or deselected (if the positive Argument is False)
eitherOrEditor :: alpha -> beta -> (Editor alpha, Parameters) ->
                        (Editor beta, Parameters) -> Text -> Editor (Either alpha beta)
eitherOrEditor alphaDefault betaDefault (leftEditor,leftParams) (rightEditor,rightParams)
            _label2 params notifier = do
    coreRef <- newIORef Nothing
    noti1 <- emptyNotifier
    noti2 <- emptyNotifier
    noti3 <- emptyNotifier
    mapM_ (propagateEvent notifier [noti1,noti2,noti3]) allGUIEvents
    be@(boolFrame,inj1,_ext1) <- boolEditor2  (getParameter paraName rightParams) leftParams noti1
    le@(leftFrame,inj2,_ext2) <- leftEditor (paraName <<<- ParaName "" $ leftParams) noti2
    re@(rightFrame,inj3,_ext3) <- rightEditor (paraName <<<- ParaName "" $ rightParams) noti3
    mkEditor
        (\widget v -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    _ <- registerEvent noti1 Clicked (onClickedHandler widget coreRef)
                    grid <- gridNew
                    orientableSetOrientation grid $ getParameter paraOrientation params
                    gridSetColumnSpacing grid 1
                    gridSetRowSpacing grid 1
                    setPrimaryAlign grid boolFrame AlignStart
                    setPrimaryAlign grid leftFrame AlignStart
                    setPrimaryAlign grid rightFrame AlignStart
                    containerAdd grid boolFrame
                    containerAdd widget grid
                    case v of
                        Left vl -> do
                          containerAdd grid leftFrame
                          inj2 vl
                          inj3 betaDefault
                          inj1 True
                        Right vr  -> do
                          containerAdd grid rightFrame
                          inj3 vr
                          inj2 alphaDefault
                          inj1 False
                    writeIORef coreRef (Just (be,le,re,grid))
                Just ((_,inj1',_),(leftFrame',inj2',_),(rightFrame',inj3',_),grid) ->
                    case v of
                            Left vl -> do
                              containerRemove grid rightFrame'
                              containerAdd grid leftFrame'
                              inj2' vl
                              inj3' betaDefault
                              inj1' True
                            Right vr  -> do
                              containerRemove grid leftFrame'
                              containerAdd grid rightFrame'
                              inj3' vr
                              inj2' alphaDefault
                              inj1' False)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2),(_,_,ext3),_) -> do
                    mbbool <- ext1
                    case mbbool of
                        Nothing -> return Nothing
                        Just True ->
                            ext2 >>= \case
                                Nothing -> return Nothing
                                Just value -> return (Just (Left value))
                        Just False ->
                            ext3 >>= \case
                                Nothing -> return Nothing
                                Just value -> return (Just (Right value)))
        (paraName <<<- ParaName "" $ params)
        notifier
    where
    onClickedHandler _widget coreRef event =  do
        core <- readIORef coreRef
        case core of
            Nothing  -> error "Impossible"
            Just ((_,_,ext1),(leftFrame,_,_),(rightFrame,_,_),grid) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->
                            if bool then do
                              containerRemove grid rightFrame
                              containerAdd grid leftFrame
                              widgetShowAll grid
                            else do
                              containerRemove grid leftFrame
                              containerAdd grid rightFrame
                              widgetShowAll grid
                    Nothing -> return ()
                return event{gtkReturn=True}


-- a trivial example: (ColumnDescr False [("",(\row -> [cellText := show row]))])
-- and a nontrivial:
--  [("Package",\(Dependency str _) -> [cellText := str])
--  ,("Version",\(Dependency _ vers) -> [cellText := showVersionRange vers])])
data ColumnDescr row = ColumnDescr Bool [(Text, CellRendererText -> row -> IO ())]

--
-- | An editor with a subeditor, of which a list of items can be selected
multisetEditor :: (Show alpha, Eq alpha) => alpha
    -> ColumnDescr alpha
    -> (Editor alpha, Parameters)
    -> Maybe ([alpha] -> [alpha]) -- ^ The 'mbSort' arg, a sort function if desired
    -> Maybe (alpha -> alpha -> Bool) -- ^ The 'mbReplace' arg, a function which is a criteria for removing an
                              --   old entry when adding a new value
    -> Editor [alpha]
multisetEditor initialValue (ColumnDescr showHeaders columnsDD) (singleEditor, sParams) mbSort mbReplace
        params notifier = do
    coreRef <- newIORef Nothing
    cnoti   <- emptyNotifier
    mkEditor
        (\widget vs -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    let orientation = getParameter paraOrientation params
                    grid <- gridNew
                    orientableSetOrientation grid orientation
                    buttonBox <- case orientation of
                        OrientationHorizontal -> buttonBoxNew OrientationVertical
                        OrientationVertical -> buttonBoxNew OrientationHorizontal
                        _ -> error "Invalid Orientation"
                    (frameS,injS,extS) <- singleEditor sParams cnoti
                    mapM_ (propagateEvent notifier [cnoti]) allGUIEvents
                    addButton    <- buttonNewWithLabel "Add"
                    removeButton <- buttonNewWithLabel "Remove"
                    containerAdd buttonBox addButton
                    containerAdd buttonBox removeButton
                    seqStore <- seqStoreNew ([]:: [alpha])
                    activateEvent seqStore notifier
                        (Just (\ w h -> afterTreeModelRowInserted w (\ _ _ -> void h))) MayHaveChanged
                    activateEvent seqStore notifier
                        (Just (\ w h -> afterTreeModelRowDeleted w (\ _ -> void h))) MayHaveChanged
                    treeView    <-  treeViewNewWithModel seqStore
                    let minSize =   getParameter paraMinSize params
                    uncurry (widgetSetSizeRequest treeView) minSize
                    sw          <-  scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
                    containerAdd sw treeView
                    scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic
                    scrolledWindowSetMinContentHeight sw (snd minSize)
                    sel         <-  treeViewGetSelection treeView
                    treeSelectionSetMode sel SelectionModeSingle
                    mapM_ (\(str,func) -> do
                            col <- treeViewColumnNew
                            treeViewColumnSetTitle  col str
                            treeViewColumnSetResizable col True
                            _ <- treeViewAppendColumn treeView col
                            renderer <- cellRendererTextNew
                            cellLayoutPackStart col renderer True
                            cellLayoutSetDataFunction col renderer seqStore (func renderer)
                        ) columnsDD
                    treeViewSetHeadersVisible treeView showHeaders
                    _ <- onTreeSelectionChanged sel $ selectionHandler sel seqStore injS
                    setPrimaryExpand grid sw True
                    setSecondaryExpand grid sw True
                    containerAdd grid sw
                    setPrimaryAlign grid buttonBox AlignEnd
                    setSecondaryExpand grid buttonBox True
                    containerAdd grid buttonBox
                    setPrimaryAlign grid frameS AlignEnd
                    setSecondaryExpand grid frameS True
                    containerAdd grid frameS
                    activateEvent treeView notifier Nothing FocusOut
                    containerAdd widget grid
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore)
                        (case mbSort of
                            Nothing -> vs
                            Just sortF -> sortF vs)
                    _ <- onButtonClicked addButton $ do
                        mbv <- extS
                        case mbv of
                            Just v -> do
                                case mbReplace of
                                    Nothing         -> return ()
                                    Just replaceF   -> do
                                         cont <- seqStoreToList seqStore
                                         mapM_ (seqStoreRemove seqStore . fst)
                                            . filter (\(_,e) -> replaceF v e)
                                                $ zip [0..] cont
                                case mbSort of
                                    Nothing    ->
                                        void $ seqStoreAppend seqStore v
                                    Just sortF -> do
                                        cont <- seqStoreToList seqStore
                                        seqStoreClear seqStore
                                        mapM_ (seqStoreAppend seqStore) (sortF (v:cont))
                                cont <- seqStoreToList seqStore
                                case elemIndex v cont of
                                    Just idx -> do
                                        path <- treePathNewFromIndices' [fromIntegral idx]
                                        treeSelectionSelectPath sel path
                                        treeViewScrollToCell treeView (Just path) (Nothing :: Maybe TreeViewColumn) False 0.0 0.0
                                    Nothing -> return ()
                            Nothing -> return ()
                    _ <- onButtonClicked removeButton $ do
                        mbi <- treeSelectionGetSelected sel
                        case mbi of
                            (True, _, iter) -> do
                                Just [i] <- treeModelGetPath seqStore iter >>= treePathGetIndices
                                seqStoreRemove seqStore i
                            _ -> return ()
                    writeIORef coreRef (Just seqStore)
                    injS initialValue
                Just seqStore -> do
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore)
                        (case mbSort of
                            Nothing -> vs
                            Just sortF -> sortF vs))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just seqStore -> do
                    v <- seqStoreToList seqStore
                    return (Just v))
        (paraMinSize <<<- ParaMinSize (-1,-1) $ params)
        notifier
    where
--    selectionHandler :: TreeSelection -> SeqStore a -> Injector a -> IO ()
    selectionHandler sel seqStore inj = do
        ts <- treeSelectionGetSelected sel
        case ts of
            (True, _, iter) -> do
                Just [i] <- treeModelGetPath seqStore iter >>= treePathGetIndices
                v <- seqStoreGetValue seqStore i
                void $ inj v
            _ -> return ()


filesEditor :: Maybe FilePath -> FileChooserAction -> Text -> Editor [FilePath]
filesEditor fp act label p =
    multisetEditor
        ""
        (ColumnDescr False [("", \cell -> setCellRendererTextText cell . T.pack)])
        (fileEditor fp act label, emptyParams)
        (Just sort)
        (Just (==))
        (paraShadow <<<- ParaShadow ShadowTypeIn $
            paraOrientation  <<<- ParaOrientation OrientationVertical $ p)

textsEditor :: (Text -> Bool) -> Bool -> Editor [Text]
textsEditor validation trimBlanks p =
    multisetEditor
        ""
        (ColumnDescr False [("", setCellRendererTextText)])
        (textEditor validation trimBlanks, emptyParams)
        (Just sort)
        (Just (==))
        (paraShadow <<<- ParaShadow ShadowTypeIn $ p)

dependencyEditor :: [PackageIdentifier] -> Editor Dependency
dependencyEditor packages para noti = do
    (wid,inj,ext) <- pairEditor
        (comboEntryEditor ((sort . nub) (map (T.pack . display . pkgName) packages))
            , paraName <<<- ParaName "Select" $ emptyParams)
        (versionRangeEditor,paraName <<<- ParaName "Version" $ emptyParams)
        (paraOrientation <<<- ParaOrientation OrientationVertical $ para)
        noti
    let pinj (viewDependency -> (pn, v, _)) = inj (T.pack (unPackageName pn),v)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just ("",_v) -> return Nothing
            Just (s,v) -> return (Just $ mkDependency (mkPackageName (T.unpack s)) v (S.singleton LMainLibName))
    return (wid,pinj,pext)

dependenciesEditor :: [PackageIdentifier] -> Editor [Dependency]
dependenciesEditor packages p =
    multisetEditor
        def
        (ColumnDescr True [("Package",\cell (viewDependency -> (pn, _, _)) -> setCellRendererTextText cell $ T.pack (unPackageName pn))
                           ,("Version",\cell (viewDependency -> (_, vers, _)) -> setCellRendererTextText cell $ T.pack $ display vers)])
        (dependencyEditor packages,
            paraHAlign <<<- ParaHAlign AlignFill
                $ paraVAlign <<<- ParaVAlign AlignCenter
                   $ emptyParams)
        (Just (sortBy (\ (viewDependency -> (p1, _, _)) (viewDependency -> (p2, _, _)) -> compare p1 p2)))
        (Just (\ (viewDependency -> (p1, _, _)) (viewDependency -> (p2, _, _)) -> p1 == p2))
        (paraShadow <<<- ParaShadow ShadowTypeIn
            $ paraHAlign <<<- ParaHAlign AlignFill
                $ paraVAlign <<<- ParaVAlign AlignCenter
                    $ paraOrientation <<<- ParaOrientation OrientationVertical
                        $ paraPack <<<- ParaPack PackGrow
                            $ p)

versionRangeEditor :: Editor VersionRange
versionRangeEditor para noti = do
    (wid,inj,ext) <-
        maybeEditor (Left def)
            (eitherOrEditor def def
               (pairEditor (comboSelectionEditor v1 (T.pack . show), emptyParams)
                  (versionEditor,
                   paraName <<<- ParaName "Enter Version" $ emptyParams),
                paraOrientation <<<- ParaOrientation OrientationVertical $
                   paraName <<<- ParaName "Simple" $
                      paraHAlign <<<- ParaHAlign AlignStart $
                         paraVAlign <<<- ParaVAlign AlignStart $
                            paraMargin <<<- ParaMargin (0, 0, 0, 0) $ emptyParams)
               (tupel3Editor
                  (comboSelectionEditor v2 (T.pack . show), emptyParams)
                  (versionRangeEditor,
                   paraShadow <<<- ParaShadow ShadowTypeIn $ emptyParams)
                  (versionRangeEditor,
                   paraShadow <<<- ParaShadow ShadowTypeIn $ emptyParams),
                paraName <<<- ParaName "Complex" $
                   paraOrientation <<<- ParaOrientation OrientationVertical $
                      paraHAlign <<<- ParaHAlign AlignStart $
                         paraVAlign <<<- ParaVAlign AlignStart $
                            paraMargin <<<- ParaMargin (0, 0, 0, 0) $ emptyParams)
               "Select version range",
             emptyParams)
            False "Any Version"
            (paraOrientation <<<- ParaOrientation OrientationVertical $ para)
            noti
    let vrinj = inj . snd . cataVersionRange (\case
                  AnyVersionF              -> (anyVersion, Nothing)
                  (ThisVersionF v)         -> (thisVersion v, Just (Left (ThisVersionS,v)))
                  (LaterVersionF v)        -> (laterVersion v, Just (Left (LaterVersionS,v)))
                  (EarlierVersionF v)      -> (earlierVersion v, Just (Left (EarlierVersionS,v)))
                  (OrLaterVersionF v)      -> (unionVersionRanges (thisVersion v) (laterVersion v), Just (Left (ThisOrLaterVersionS,v)))
                  (OrEarlierVersionF v)    -> (unionVersionRanges (thisVersion v) (earlierVersion v), Just (Left (ThisOrEarlierVersionS,v)))
                  (WildcardVersionF v1')   -> (withinVersion v1', Just (Left (WildcardVersionS,v1')))
                  (MajorBoundVersionF v1') -> (majorBoundVersion v1', Just (Left (MajorBoundVersionS,v1')))
                  (UnionVersionRangesF (vr1, _r1) (vr2, _r2))
                                           -> (unionVersionRanges vr1 vr2, Just (Right (UnionVersionRangesS,vr1,vr2)))
                  (IntersectVersionRangesF (vr1, _r1) (vr2, _r2))
                                           -> (intersectVersionRanges vr1 vr2, Just (Right (IntersectVersionRangesS,vr1,vr2)))
          ) . normaliseVersionRange
    let vrext = do  mvr <- ext
                    case mvr of
                        Nothing -> return (Just anyVersion)
                        Just Nothing -> return (Just anyVersion)
                        Just (Just (Left (ThisVersionS,v)))          -> return (Just (thisVersion v))
                        Just (Just (Left (WildcardVersionS,v)))      -> return (Just (withinVersion v))
                        Just (Just (Left (MajorBoundVersionS,v)))    -> return (Just (majorBoundVersion v))
                        Just (Just (Left (LaterVersionS,v)))         -> return (Just (laterVersion v))
                        Just (Just (Left (EarlierVersionS,v)))       -> return (Just (earlierVersion v))

                        Just (Just (Left (ThisOrLaterVersionS,v)))   -> return (Just (orLaterVersion v))
                        Just (Just (Left (ThisOrEarlierVersionS,v))) -> return (Just (orEarlierVersion v))
                        Just (Just (Right (UnionVersionRangesS,v1',v2')))
                                                        -> return (Just (unionVersionRanges v1' v2'))
                        Just (Just (Right (IntersectVersionRangesS,v1',v2')))
                                                        -> return (Just (intersectVersionRanges v1' v2'))
    return (wid,vrinj,vrext)
        where
            v1 = [ThisVersionS,WildcardVersionS
                 ,MajorBoundVersionS
                 ,LaterVersionS,ThisOrLaterVersionS,EarlierVersionS,ThisOrEarlierVersionS]
            v2 = [UnionVersionRangesS,IntersectVersionRangesS]

data Version1 = ThisVersionS | WildcardVersionS | MajorBoundVersionS | LaterVersionS | ThisOrLaterVersionS | EarlierVersionS | ThisOrEarlierVersionS
    deriving (Eq)
instance Show Version1 where
    show ThisVersionS          = "This Version"
    show WildcardVersionS      = "Wildcard Version"
    show MajorBoundVersionS    = "Major Bound Version"
    show LaterVersionS         = "Later Version"
    show ThisOrLaterVersionS   = "This or later Version"
    show EarlierVersionS       = "Earlier Version"
    show ThisOrEarlierVersionS = "This or earlier Version"

data Version2 = UnionVersionRangesS | IntersectVersionRangesS
    deriving (Eq)
instance Show Version2 where
    show UnionVersionRangesS     = "Union Version Ranges"
    show IntersectVersionRangesS = "Intersect Version Ranges"

versionEditor :: Editor Version
versionEditor para noti = do
    (wid,inj,ext) <- stringEditor (not . null) True para noti
    let pinj v = inj (display v)
    let pext =
          ext >>= \case
              Nothing -> return Nothing
              Just s -> return (simpleParse s)
    return (wid, pinj, pext)

instance Default Version1
    where def = ThisVersionS

instance Default Version2
    where def = UnionVersionRangesS

instance Default Version
    where def = fromJust (simpleParse "0")

instance Default VersionRange
    where def = anyVersion

instance Default Dependency
    where def = Dependency def def
#if MIN_VERSION_Cabal (3,0,0)
                    (S.singleton LMainLibName)
#endif

instance Default PackageName
    where def = mkPackageName def






