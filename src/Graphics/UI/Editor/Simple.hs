{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.Simple
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for making simple editors
--
-----------------------------------------------------------------------------------

module Graphics.UI.Editor.Simple (
    noEditor
,   boolEditor
,   boolEditor2
,   enumEditor
,   clickEditor
,   textEditor
,   stringEditor
,   multilineStringEditor
,   intEditor
,   genericEditor
,   buttonEditor
,   fontEditor
,   colorEditor
,   comboSelectionEditor
,   comboEntryEditor
,   staticListEditor
,   staticListMultiEditor
,   multiselectionEditor
,   fileEditor
,   otherEditor
,   imageEditor

,   okCancelFields

,   Color(..)
,   toGdkColor
,   fromGdkColor
) where

import Prelude ()
import Prelude.Compat
import Control.Monad (void, when)
import Data.IORef
import Data.List (elemIndex)
import Data.Maybe
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

import Graphics.UI.Editor.Parameters
--import Graphics.UI.Editor.Basics
import Graphics.UI.Editor.MakeEditor
import Control.Event
import Unsafe.Coerce (unsafeCoerce)
import Graphics.UI.Editor.Basics
       (GUIEvent(..), GUIEventSelector(..), propagateAsChanged,
        genericGUIEvents, activateEvent, Editor)
import Control.Exception as E (catch, IOException)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as T (strip, unpack, pack, empty)
import Data.Text (Text)
import Data.GI.Base (unsafeCastTo)
import Data.GI.Base.GObject (new')
import GI.Gtk
       (orientableSetOrientation, colorChooserGetRgba,
        colorChooserSetRgba, widgetSetHalign, imageSetFromIconName,
        imageNewFromIconName, gridNew, Adjustment,
        setCellRendererToggleActive, RadioButton, onColorButtonColorSet,
        colorButtonNew, fontButtonGetFontName, fontButtonSetFontName,
        onFontButtonFontSet, fontButtonNew, widgetDestroy,
        fileChooserGetFilename, dialogRun, widgetShow,
        fileChooserSetAction, dialogAddButton, setWindowTitle,
        FileChooserDialog(..), FileChooserAction, treeSelectionSelectPath,
        onWidgetKeyPressEvent, onCellRendererToggleToggled,
        scrolledWindowSetMinContentHeight,
        setCellRendererToggleActivatable, cellRendererToggleNew,
        treeSelectionUnselectAll, constructDialogUseHeaderBar,
        treeViewSetHeadersVisible, setCellRendererTextText,
        cellLayoutPackStart, treeViewAppendColumn, treeViewColumnNew,
        cellRendererTextNew, treeSelectionSetMode, treeViewGetSelection,
        treeViewNewWithModel, Entry(..), binGetChild,
        comboBoxTextNewWithEntry, comboBoxGetActive, comboBoxSetActive,
        onComboBoxChanged, widgetSetSizeRequest, spinButtonGetValue,
        spinButtonSetValue, afterSpinButtonChangeValue,
        spinButtonNewWithRange, textBufferGetText, textBufferGetEndIter,
        textBufferGetStartIter, textBufferSetText, textViewGetBuffer,
        scrolledWindowSetPolicy, scrolledWindowNew, textViewNew,
        entryGetText, entrySetText, entryNew, setWidgetCanDefault,
        widgetGrabDefault, buttonNewWithLabel, widgetSetName,
        radioButtonNewWithLabelFromWidget, radioButtonNewWithLabel,
        toggleButtonGetActive, toggleButtonSetActive, setWidgetName,
        checkButtonNewWithLabel, containerAdd, onWidgetScrollEvent,
        comboBoxTextAppendText)
import GI.Gtk.Enums
       (Orientation(..), Align(..), ResponseType(..),
        SelectionMode(..), PolicyType(..), IconSize(..))
import Data.GI.Gtk.ComboBox
       (comboBoxAppendText, comboBoxNewText)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetDataFunction)
import Data.GI.Gtk.ModelView.SeqStore
       (seqStoreNew, seqStoreToList, seqStoreGetValue,
        seqStoreAppend, seqStoreClear, seqStoreSetValue)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathGetIndices',
        treePathNewFromIndices', stringToTreePath)
import GI.Gdk
       (getRGBABlue, getRGBAGreen, getRGBARed, setRGBAAlpha, setRGBABlue,
        setRGBAGreen, setRGBARed, newZeroRGBA, getColorBlue, getColorGreen,
        getColorRed, setColorBlue, setColorGreen, setColorRed, RGBA,
        keyvalName, getEventKeyKeyval)
import GI.GObject (signalStopEmissionByName)
import Data.Word (Word16)
import qualified GI.Gdk as Gdk (Color(..))
import Data.GI.Base.Constructible (Constructible(..))

-- ------------------------------------------------------------
-- * Simple Editors
-- ------------------------------------------------------------

--
-- | An invisible editor without any effect
--
noEditor :: alpha -> Editor alpha
noEditor proto =
    mkEditor
        (\_widget _ -> return ())
        (return (Just proto))

--
-- | Editor for a boolean value in the form of a check button
--
boolEditor :: Editor Bool
boolEditor params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget bool -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- checkButtonNewWithLabel (getParameter paraName params)
                    setWidgetName button (getParameter paraName params)
                    containerAdd widget button
                    toggleButtonSetActive button bool
                    mapM_ (activateEvent button notifier Nothing)
                        (Clicked: genericGUIEvents)
                    propagateAsChanged notifier [Clicked]
                    writeIORef coreRef (Just button)
                Just button -> toggleButtonSetActive button bool)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just button -> do
                    r <- toggleButtonGetActive button
                    return (Just r))
        (paraName <<<- ParaName T.empty $ params)
        notifier

--
-- | Editor for a boolean value in the form of two radio buttons
----
boolEditor2 :: Text -> Editor Bool
boolEditor2 label2 params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget bool -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    grid <- gridNew
                    orientableSetOrientation grid OrientationVertical
                    radio1 <- radioButtonNewWithLabel ([]::[RadioButton]) (getParameter paraName params)
                    radio2 <- radioButtonNewWithLabelFromWidget (Just radio1) label2
                    containerAdd grid radio1
                    containerAdd grid radio2
                    widgetSetName radio1 $ getParameter paraName params <> ".1"
                    widgetSetName radio2 $ getParameter paraName params <> ".2"
                    containerAdd widget grid
                    if bool
                        then toggleButtonSetActive radio1 True
                        else toggleButtonSetActive radio2 True
                    mapM_ (activateEvent radio1 notifier Nothing) (Clicked:genericGUIEvents)
                    mapM_ (activateEvent radio2 notifier Nothing) (Clicked:genericGUIEvents)
                    propagateAsChanged notifier [Clicked]
                    writeIORef coreRef (Just (radio1,radio2))
                Just (radio1,radio2) ->
                    if bool
                        then toggleButtonSetActive radio1 True
                        else toggleButtonSetActive radio2 True)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (radio1,_radio2) -> do
                    r <- toggleButtonGetActive radio1
                    return (Just r))
        (paraName <<<- ParaName "" $ params)
        notifier

--
-- | Editor for an enum value in the form of n radio buttons
----
enumEditor :: forall alpha . (Show alpha, Enum alpha, Bounded alpha)  => [Text] -> Editor alpha
enumEditor labels params notifier = do
    coreRef <- newIORef Nothing
    let vals :: [alpha] = [minBound .. maxBound]
    mkEditor
        (\widget enumValue -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    grid <- gridNew
                    orientableSetOrientation grid OrientationVertical
                    let label0 = case labels ++ map (T.pack . show) vals of
                                    (x:_) -> x
                                    _     -> error "enumEditor"
                    button0 <- radioButtonNewWithLabel ([]::[RadioButton]) label0
                    buttons <- mapM (\ v -> do
                        let n = fromEnum v
                        let label = case drop n labels of
                                        (l:_) -> l
                                        _     -> T.pack (show v)
                        radio <- if n == 0
                                    then return button0
                                    else radioButtonNewWithLabelFromWidget (Just button0) label
                        containerAdd grid radio
                        widgetSetName radio (label <> T.pack (show n))
                        return radio) vals
                    containerAdd widget grid
                    mapM_
                        (\e ->
                            (mapM_
                                (\b -> activateEvent b notifier Nothing e)
                             buttons)) (Clicked:genericGUIEvents)
                    propagateAsChanged notifier [Clicked]
                    mapM_ (\(b,n) -> toggleButtonSetActive b (n == fromEnum enumValue))
                                (zip buttons [0..])
                    writeIORef coreRef (Just buttons)
                Just buttons ->
                    mapM_ (\(b,n) -> toggleButtonSetActive b (n == fromEnum enumValue))
                                (zip buttons [0..]))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just buttons -> do
                    boolArray <- mapM toggleButtonGetActive buttons
                    let mbInd =  elemIndex True boolArray
                    let res = case mbInd of
                                Nothing -> Nothing
                                Just i -> Just (vals !! i)
                    return res)
        (paraName <<<- ParaName "" $ params)
        notifier

-- | An Editor for nothing (which may report a click) in the form of a button
--
clickEditor :: Bool -> Editor ()
clickEditor canDefault params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget _bool -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel (getParameter paraName params)
                    containerAdd widget button
                    activateEvent button notifier Nothing Clicked
                    writeIORef coreRef (Just button)
                    when canDefault $ do
                        setWidgetCanDefault button True
                        widgetGrabDefault button
                Just _button -> return ())
        (return (Just ()))
        (paraName <<<- ParaName "" $ params)
        notifier

-- | An Editor to display an image
--
imageEditor :: Editor Text
imageEditor params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget stockId -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    image <- imageNewFromIconName (Just stockId) (fromIntegral $ fromEnum IconSizeLargeToolbar)
                    widgetSetName image (getParameter paraName params)
                    containerAdd widget image
                    writeIORef coreRef (Just (image,stockId))
                Just (image, _stockId2) -> imageSetFromIconName image (Just stockId) (fromIntegral $ fromEnum IconSizeLargeToolbar))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_,stockId3) -> return (Just stockId3))
        params
        notifier

--
-- | Editor for a Text in the form of a text entry
--
textEditor :: (Text -> Bool) -> Bool -> Editor Text
textEditor validation trimBlanks params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget string -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    entry   <-  entryNew
                    widgetSetName entry (getParameter paraName params)
                    mapM_ (activateEvent entry notifier Nothing) genericGUIEvents
                    propagateAsChanged notifier [KeyPressed]
                    containerAdd widget entry
                    entrySetText entry (if trimBlanks then T.strip string else string)
                    writeIORef coreRef (Just entry)
                Just entry -> entrySetText entry (if trimBlanks then T.strip string else string))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just entry -> do
                    r <- entryGetText entry
                    if validation r
                        then return (Just (if trimBlanks then T.strip r else r))
                        else return Nothing)
        params
        notifier

--
-- | Editor for a String in the form of a text entry
--
stringEditor :: (String -> Bool) -> Bool -> Editor String
stringEditor validation _trimBlanks params notifier = do
    (wid,inj,ext) <- textEditor (validation . T.unpack) True params notifier
    return (wid, inj . T.pack, (T.unpack <$>) <$> ext)

--
-- | Editor for a multiline string in the form of a multiline text entry
--
multilineStringEditor :: Editor Text
multilineStringEditor params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget string -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    aTextView       <-  textViewNew
                    widgetSetName aTextView (getParameter paraName params)
                    aScrolledWindow <-  scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
                    scrolledWindowSetPolicy aScrolledWindow PolicyTypeAutomatic PolicyTypeAutomatic
                    containerAdd aScrolledWindow aTextView
                    containerAdd widget aScrolledWindow
                    mapM_ (activateEvent aTextView notifier Nothing) genericGUIEvents
                    propagateAsChanged notifier [KeyPressed]
                    buffer          <-  textViewGetBuffer aTextView
                    textBufferSetText buffer string (-1)
                    writeIORef coreRef (Just (aScrolledWindow,aTextView))
                Just (_aScrolledWindow,aTextView) -> do
                    buffer          <-  textViewGetBuffer aTextView
                    textBufferSetText buffer string (-1))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_aScrolledWindow, aTextView) -> do
                    buffer          <-  textViewGetBuffer aTextView
                    start           <-  textBufferGetStartIter buffer
                    end             <-  textBufferGetEndIter buffer
                    r               <-  textBufferGetText buffer start end False
                    return (Just r))
        params
        notifier

--
-- | Editor for an integer in the form of a spin entry
--
intEditor :: (Double,Double,Double) -> Editor Int
intEditor (minValue, maxValue, step) params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget v -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    spin <- spinButtonNewWithRange minValue maxValue step
                    widgetSetName spin (getParameter paraName params)
                    -- ignore scroll event, propagate to parent widget
                    _ <- onWidgetScrollEvent spin (\_ -> signalStopEmissionByName spin "scroll-event" >> return False)
                    mapM_ (activateEvent spin notifier Nothing) genericGUIEvents
                    activateEvent spin notifier
                        (Just (\ w h ->
                            afterSpinButtonChangeValue w (\_ -> void h))) MayHaveChanged
                    containerAdd widget spin
                    spinButtonSetValue spin (fromIntegral v)
                    writeIORef coreRef (Just spin)
                Just spin -> spinButtonSetValue spin (fromIntegral v))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just spin -> do
                    newNum <- spinButtonGetValue spin
                    return (Just (truncate newNum)))
        params
        notifier

--
-- | Editor for for any value which is an instance of Read and Show in the form of a
-- | text entry
genericEditor :: (Show beta, Read beta) => Editor beta
genericEditor params notifier = do
    (wid,inj,ext) <- stringEditor (const True) True params notifier
    let ginj = inj . show
    let gext = ext >>= \case
            Nothing -> return Nothing
            Just s -> E.catch (Just <$> readIO s)
                            (\(e :: IOException) -> do
                                putStrLn ("Generic editor no parse for " ++ s ++ " " ++ show e)
                                return Nothing)
    return (wid,ginj,gext)

--
-- | Editor for no value, it only emtis a clicked event and has the form of a check button
--
buttonEditor :: Editor ()
buttonEditor params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget _ -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel (getParameter paraName params)
                    widgetSetName button (getParameter paraName params)
                    containerAdd widget button
                    mapM_ (activateEvent button notifier Nothing) (Clicked:genericGUIEvents)
                    writeIORef coreRef (Just button)
                Just _button -> return ())
        (return (Just ()))
        params
        notifier

--
-- | Editor for the selection of some element from a static list of elements in the
-- | form of a combo box
comboSelectionEditor :: Eq beta => [beta] -> (beta -> Text) -> Editor beta
comboSelectionEditor list showF params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget obj -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    combo <- comboBoxNewText
                    widgetSetSizeRequest combo 200 (-1)
                    -- ignore scroll event, propagate to parent widget
                    _ <- onWidgetScrollEvent combo (\_ -> signalStopEmissionByName combo "scroll-event" >> return False)
                    mapM_ (comboBoxAppendText combo . showF) list
                    widgetSetName combo (getParameter paraName params)
                    mapM_ (activateEvent combo notifier Nothing) genericGUIEvents
                    activateEvent combo notifier
                        (Just (\ w h -> do
                            res     <-  onComboBoxChanged w (void h)
                            return (unsafeCoerce res))) MayHaveChanged
                    comboBoxSetActive combo 1
                    containerAdd widget combo
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> comboBoxSetActive combo (fromIntegral i)
                        Nothing -> return ()
                    writeIORef coreRef (Just combo)
                Just combo -> do
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> comboBoxSetActive combo (fromIntegral i)
                        Nothing -> return ())
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just combo -> do
                    ind <- comboBoxGetActive combo
                    return $ case ind of
                        (-1) -> Nothing
                        _    -> Just (list !! fromIntegral ind))
        params
        notifier

-- | Like comboSelectionEditor but allows entry of text not in the list
comboEntryEditor :: [Text] -> Editor Text
comboEntryEditor list params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget obj -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    combo <- comboBoxTextNewWithEntry
                    widgetSetSizeRequest combo 200 (-1)
                    mapM_ (comboBoxTextAppendText combo) list
                    widgetSetName combo (getParameter paraName params)
                    mapM_ (activateEvent combo notifier Nothing) genericGUIEvents
                    activateEvent combo notifier
                        (Just (\ w h -> do
                            res     <-  onComboBoxChanged w (void h)
                            return (unsafeCoerce res))) MayHaveChanged
                    containerAdd widget combo
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> comboBoxSetActive combo (fromIntegral i)
                        Nothing -> do
                            entry <- (fromJust <$> binGetChild combo) >>= unsafeCastTo Entry
                            entrySetText entry obj
                    writeIORef coreRef (Just combo)
                Just combo -> do
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> comboBoxSetActive combo (fromIntegral i)
                        Nothing -> return ())
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just combo -> do
                    entry <- (fromJust <$> binGetChild combo) >>= unsafeCastTo Entry
                    Just <$> entryGetText entry)
        params
        notifier

--
-- | Editor for the selection of some elements from a list of elements in the
-- | form of a list box
multiselectionEditor :: (Show beta, Eq beta) => Editor [beta]
multiselectionEditor params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget objs -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    seqStore   <- seqStoreNew []
                    listView    <- treeViewNewWithModel seqStore
                    widgetSetName listView (getParameter paraName params)
                    mapM_ (activateEvent listView notifier Nothing) genericGUIEvents
                    propagateAsChanged notifier [KeyPressed,ButtonPressed]
                    sel         <- treeViewGetSelection listView
                    treeSelectionSetMode sel SelectionModeMultiple
                    renderer    <- cellRendererTextNew
                    col         <- treeViewColumnNew
                    _ <- treeViewAppendColumn listView col
                    cellLayoutPackStart col renderer True
                    cellLayoutSetDataFunction col renderer seqStore
                        (setCellRendererTextText renderer . T.pack . show)
                    treeViewSetHeadersVisible listView False
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore) objs
                    containerAdd widget listView
                    treeSelectionUnselectAll sel
                    --let inds = catMaybes $map (\obj -> elemIndex obj list) objs
                    --mapM_ (\i -> treeSelectionSelectPath sel [i]) inds
                    writeIORef coreRef (Just (listView,seqStore))
                Just (_listView, seqStore) -> do
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore) objs)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (listView,seqStore) -> do
                    sel         <- treeViewGetSelection listView
                    treePaths   <- treeSelectionGetSelectedRows' sel >>= mapM treePathGetIndices'
                    values      <- mapM (\[i] -> seqStoreGetValue seqStore (fromIntegral i)) treePaths
                    return (Just values))
        params
        notifier

--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box with toggle elements

staticListMultiEditor :: (Eq beta) => [beta] -> (beta -> Text) -> Editor [beta]
staticListMultiEditor list showF params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget objs -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    seqStore <- seqStoreNew ([]:: [(Bool,beta)])
                    listView <- treeViewNewWithModel seqStore
                    widgetSetName listView (getParameter paraName params)
                    mapM_ (activateEvent listView notifier Nothing) genericGUIEvents
                    propagateAsChanged notifier [KeyPressed,ButtonPressed]
                    sel <- treeViewGetSelection listView
                    treeSelectionSetMode sel SelectionModeSingle
                    rendererToggle <- cellRendererToggleNew
                    setCellRendererToggleActivatable rendererToggle True
                    rendererText <- cellRendererTextNew
                    col1 <- treeViewColumnNew
                    _ <- treeViewAppendColumn listView col1
                    cellLayoutPackStart col1 rendererToggle True
                    cellLayoutSetDataFunction col1 rendererToggle seqStore
                        (setCellRendererToggleActive rendererToggle . fst)
                    col2 <- treeViewColumnNew
                    _ <- treeViewAppendColumn listView col2
                    cellLayoutPackStart col2 rendererText True
                    cellLayoutSetDataFunction col2 rendererText seqStore
                        (setCellRendererTextText rendererText . showF . snd)
                    treeViewSetHeadersVisible listView False
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore . (\ e -> (e `elem` objs, e))) list
                    let minSize =   getParameter paraMinSize params
                    uncurry (widgetSetSizeRequest listView) minSize
                    sw <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
                    containerAdd sw listView
                    scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic
                    containerAdd widget sw
                    scrolledWindowSetMinContentHeight sw (snd minSize)
                      -- update the model when the toggle buttons are activated
                    _ <- onCellRendererToggleToggled rendererToggle $ \pathStr -> do
                        let (i:_) = stringToTreePath pathStr
                        val <- seqStoreGetValue seqStore i
                        seqStoreSetValue seqStore i (not (fst val),snd val)
                    _ <- onWidgetKeyPressEvent listView $ \e -> do
                        name <- getEventKeyKeyval e >>= keyvalName
                        liftIO $
                            case name of
                                Just "Return" -> do
                                    sel' <- treeViewGetSelection listView
                                    rows <- treeSelectionGetSelectedRows' sel' >>= mapM treePathGetIndices'
                                    mapM_ (\ (i:_) -> do
                                        val <- seqStoreGetValue seqStore i
                                        seqStoreSetValue seqStore i (not (fst val),snd val)) rows
                                    return True
                                _ -> return False
                    writeIORef coreRef (Just (listView,seqStore))
                Just (_listView, seqStore) -> do
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore . (\ e -> (e `elem` objs, e))) list)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_listView, seqStore) -> do
                    model <- seqStoreToList seqStore
                    return (Just (map snd $ filter fst model)))
        params
        notifier

--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box

staticListEditor :: (Eq beta) => [beta] -> (beta -> Text) -> Editor beta
staticListEditor list showF params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget obj -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    seqStore <- seqStoreNew ([]:: [alpha])
                    listView <- treeViewNewWithModel seqStore
                    widgetSetName listView (getParameter paraName params)
                    mapM_ (activateEvent listView notifier Nothing) genericGUIEvents
                    propagateAsChanged notifier [KeyPressed,ButtonPressed]
                    sel <- treeViewGetSelection listView
                    treeSelectionSetMode sel
                        (if getParameter paraMultiSel params
                            then SelectionModeMultiple
                            else SelectionModeSingle)
                    renderer <- cellRendererTextNew
                    col <- treeViewColumnNew
                    _ <- treeViewAppendColumn listView col
                    cellLayoutPackStart col renderer True
                    cellLayoutSetDataFunction col renderer seqStore
                        (setCellRendererTextText renderer . showF)
                    treeViewSetHeadersVisible listView False
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore) list
                    let minSize =   getParameter paraMinSize params
                    uncurry (widgetSetSizeRequest listView) minSize
                    sw <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
                    containerAdd sw listView
                    scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic
                    containerAdd widget sw
                    scrolledWindowSetMinContentHeight sw (snd minSize)
                    treeSelectionUnselectAll sel
                    let mbInd = elemIndex obj list
                    case mbInd of
                        Nothing -> return ()
                        Just ind -> treeSelectionSelectPath sel =<< treePathNewFromIndices' [fromIntegral ind]
                    writeIORef coreRef (Just listView)
                Just listView -> do
                    sel <- treeViewGetSelection listView
                    treeSelectionUnselectAll sel
                    let mbInd = elemIndex obj list
                    case mbInd of
                        Nothing -> return ()
                        Just ind -> treeSelectionSelectPath sel =<< treePathNewFromIndices' [fromIntegral ind])
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just listView -> do
                    sel <- treeViewGetSelection listView
                    treePaths <- treeSelectionGetSelectedRows' sel >>= mapM treePathGetIndices'
                    case treePaths of
                        [[i]] -> return (Just (list !! fromIntegral i))
                        _ -> return Nothing)
        params
        notifier


--
-- | Editor for the selection of a file path in the form of a text entry and a button,
-- | which opens a gtk file chooser
fileEditor :: Maybe FilePath -> FileChooserAction -> Text -> Editor FilePath
fileEditor mbFilePath action buttonName params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget filePath -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel buttonName
                    widgetSetName button $ getParameter paraName params <> "-button"
                    mapM_ (activateEvent button notifier Nothing)
                        (Clicked:genericGUIEvents)
                    entry <- entryNew
                    widgetSetName entry $ getParameter paraName params <> "-entry"
                    -- set entry [ entryEditable := False ]
                    mapM_ (activateEvent entry notifier Nothing) genericGUIEvents
                    _ <- registerEvent notifier Clicked (buttonHandler entry)
                    propagateAsChanged notifier [KeyPressed,ButtonPressed]
                    grid <- gridNew
                    orientableSetOrientation grid (getParameter paraOrientation params)
                    widgetSetHalign button AlignEnd
                    containerAdd grid entry
                    containerAdd grid button
                    containerAdd widget grid
                    entrySetText entry (T.pack filePath)
                    writeIORef coreRef (Just entry)
                Just entry -> entrySetText entry (T.pack filePath))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just entry -> do
                    str <- entryGetText entry
                    return (Just (T.unpack str)))
        params
        notifier
    where
    buttonHandler entry e =  do
        mbFileName <- do
            dialog <- new' FileChooserDialog [constructDialogUseHeaderBar 1]
            setWindowTitle dialog "Select File"
            _ <- dialogAddButton dialog "gtk-cancel" (fromIntegral $ fromEnum ResponseTypeCancel)
            _ <- dialogAddButton dialog "gtk-open" (fromIntegral $ fromEnum ResponseTypeAccept)
            fileChooserSetAction dialog action
            widgetShow dialog
            response <- dialogRun dialog
            case toEnum $ fromIntegral response of
                ResponseTypeAccept -> do
                    f <- fileChooserGetFilename dialog
                    widgetDestroy dialog
                    return f
                ResponseTypeCancel -> do
                    widgetDestroy dialog
                    return Nothing
                ResponseTypeDeleteEvent-> do
                    widgetDestroy dialog
                    return Nothing
                _   -> return Nothing
        case mbFileName of
            Nothing -> return (e{gtkReturn=True})
            Just fn -> do
--                let relative = case mbFilePath of
--                                Nothing -> fn
--                                Just rel -> makeRelative rel fn
                entrySetText entry (T.pack fn)
                _ <- triggerEvent notifier GUIEvent {
                    selector = MayHaveChanged,
                    eventText = "",
                    gtkReturn = True}
                return (e{gtkReturn=True})

--
-- | Editor for a font selection
--
fontEditor :: Editor (Maybe Text)
fontEditor params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget mbValue -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    fs <- fontButtonNew
                    widgetSetName fs $ getParameter paraName params
                    mapM_ (activateEvent fs notifier Nothing) (Clicked: genericGUIEvents)
                    activateEvent fs notifier
                        (Just (\ w h -> do
                            res <- onFontButtonFontSet w (void h)
                            return (unsafeCoerce res))) MayHaveChanged
                    containerAdd widget fs
                    mapM_ (fontButtonSetFontName fs) mbValue
                    writeIORef coreRef (Just fs)
                Just fs -> mapM_ (fontButtonSetFontName fs) mbValue)
        (readIORef coreRef >>= \case
            Nothing -> return Nothing
            Just fs -> do
                f <- fontButtonGetFontName fs
                return (Just (Just f)))
        params
        notifier

data Color = Color Word16 Word16 Word16 deriving(Eq, Show, Generic)

instance ToJSON Color
instance FromJSON Color

toGdkColor :: MonadIO m => Color -> m Gdk.Color
toGdkColor (Color r g b) = do
    c <- new Gdk.Color []
    setColorRed   c r
    setColorGreen c g
    setColorBlue  c b
    return c

fromGdkColor :: MonadIO m => Gdk.Color -> m Color
fromGdkColor c = do
    r <- getColorRed c
    g <- getColorGreen c
    b <- getColorBlue c
    return $ Color r g b

toGdkRGBA :: MonadIO m => Color -> m RGBA
toGdkRGBA (Color r g b) = do
    c <- newZeroRGBA
    setRGBARed   c (fromIntegral r / 65535)
    setRGBAGreen c (fromIntegral g / 65535)
    setRGBABlue  c (fromIntegral b / 65535)
    setRGBAAlpha c 65535
    return c

fromGdkRGBA :: MonadIO m => RGBA -> m Color
fromGdkRGBA c = do
    r <- getRGBARed c
    g <- getRGBAGreen c
    b <- getRGBABlue c
    return $ Color (round (r * 65535)) (round (g * 65535)) (round (b * 65535))

--
-- | Editor for color selection
--
colorEditor :: Editor Color
colorEditor params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget c -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    cs <- colorButtonNew
                    widgetSetName cs $ getParameter paraName params
                    mapM_ (activateEvent cs notifier Nothing) (Clicked: genericGUIEvents)
                    activateEvent cs notifier
                        (Just (\ w h -> do
                            res <- onColorButtonColorSet w (void h)
                            return (unsafeCoerce res))) MayHaveChanged
                    containerAdd widget cs
                    colorChooserSetRgba cs =<< toGdkRGBA c
                    writeIORef coreRef (Just cs)
                Just cs -> colorChooserSetRgba cs =<< toGdkRGBA c)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just cs -> do
                    c <- colorChooserGetRgba cs >>= fromGdkRGBA
                    return (Just c))
        params
        notifier

--
-- | An editor, which opens another editor
--   You have to inject a value before the button can be clicked.
--
otherEditor :: (alpha  -> Text -> IO (Maybe alpha)) -> Editor alpha
otherEditor func params notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget val -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel (getParameter paraName params)
                    widgetSetName button $ getParameter paraName params
                    containerAdd widget button
                    mapM_ (activateEvent button notifier Nothing) (Clicked:genericGUIEvents)
                    _ <- registerEvent notifier Clicked (buttonHandler coreRef)
                    propagateAsChanged notifier [KeyPressed,ButtonPressed,Clicked]
                    writeIORef coreRef (Just (button,val))
                Just (button, _oldval) -> writeIORef coreRef (Just (button, val)))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_,val) -> return (Just val))
        (paraName <<<- ParaName "" $ params)
        notifier
    where
    buttonHandler coreRef e = do
        core <- readIORef coreRef
        case core of
            Nothing -> error "You have to inject a value before the button can be clicked"
            Just (b,val) -> do
                res <- func val (getParameter paraName params)
                case res of
                    Nothing     -> return (e{gtkReturn=True})
                    Just nval   -> do
                        writeIORef coreRef (Just (b, nval))
                        return (e{gtkReturn=True})

okCancelFields :: FieldDescription ()
okCancelFields = HFD emptyParams [
        mkField
            (paraName <<<- ParaName "Cancel"
                $ emptyParams)
            (const ())
            (\ _ b -> b)
            (clickEditor False)
    ,   mkField
            (paraName <<<- ParaName "Ok"
                $ emptyParams)
            (const ())
            (\ _ b -> b)
            (clickEditor True)]
