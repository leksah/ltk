{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
) where

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Data.Int (Int32)
import System.FilePath.Posix

import Graphics.UI.Editor.Parameters
--import Graphics.UI.Editor.Basics
import Graphics.UI.Editor.MakeEditor
import Control.Event
import MyMissing (allOf)
import Unsafe.Coerce (unsafeCoerce)
import Graphics.UI.Editor.Basics
       (GUIEvent(..), GUIEventSelector(..), propagateAsChanged,
        genericGUIEvents, activateEvent, Editor)
import Control.Exception as E (catch, IOException)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Applicative ((<$>))
import qualified Data.Text as T (strip, unpack, pack, empty)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.GI.Base (new, GObject(..), unsafeCastTo)
import GI.Gtk
       (noAdjustment, setCellRendererToggleActive,
        RadioButton, pattern STOCK_CANCEL, pattern STOCK_OK,
        colorButtonGetColor, colorButtonSetColor, ColorButton(..),
        onColorButtonColorSet, colorButtonNew, fontButtonGetFontName,
        fontButtonSetFontName, FontButton(..), onFontButtonFontSet,
        fontButtonNew, widgetDestroy, fileChooserGetFilename, dialogRun,
        widgetShow, fileChooserSetAction, dialogAddButton, setWindowTitle,
        FileChooserDialog(..), boxPackEnd, toBox, hBoxNew,
        FileChooserAction, treeSelectionSelectPath, onWidgetKeyPressEvent,
        onCellRendererToggleToggled, scrolledWindowSetMinContentHeight,
        setCellRendererToggleActivatable, cellRendererToggleNew,
        treeSelectionUnselectAll,
        treeViewSetHeadersVisible, setCellRendererTextText,
        cellLayoutPackStart, treeViewAppendColumn, treeViewColumnNew,
        cellRendererTextNew, treeSelectionSetMode, treeViewGetSelection,
        treeViewNewWithModel, Entry(..), binGetChild, comboBoxNewWithEntry,
        comboBoxGetActive, comboBoxSetActive, ComboBox(..),
        onComboBoxChanged, widgetSetSizeRequest, spinButtonGetValue,
        spinButtonSetValue, SpinButton(..), afterSpinButtonChangeValue,
        spinButtonNewWithRange, textBufferGetText, textBufferGetEndIter,
        textBufferGetStartIter, textBufferSetText, textViewGetBuffer,
        scrolledWindowSetPolicy, scrolledWindowNew, textViewNew,
        entryGetText, entrySetText, entryNew, imageSetFromStock,
        imageNewFromStock, setWidgetCanDefault, widgetGrabDefault,
        buttonNewFromStock, buttonNewWithLabel, widgetSetName,
        boxPackStart, radioButtonNewWithLabelFromWidget,
        radioButtonNewWithLabel, vBoxNew, toggleButtonGetActive, toWidget,
        toggleButtonSetActive, setWidgetName, checkButtonNewWithLabel,
        containerAdd)
import GI.Gtk.Enums
       (ResponseType(..), SelectionMode(..), PolicyType(..), IconSize(..))
import Data.GI.Gtk.ComboBox
       (comboBoxSetModelText, comboBoxAppendText, comboBoxNewText)
import Data.GI.Gtk.ModelView.CellLayout
       (cellLayoutSetAttributeFunc)
import Data.GI.Gtk.ModelView.CustomStore
       (CustomStore(..), customStoreGetRow)
import Data.GI.Gtk.ModelView.SeqStore
       (SeqStore(..), seqStoreNew, seqStoreToList, seqStoreGetValue,
        seqStoreAppend, seqStoreClear, seqStoreSetValue)
import Data.GI.Gtk.ModelView.Types
       (treeSelectionGetSelectedRows', treePathGetIndices',
        treePathNewFromIndices', stringToTreePath)
import GI.Gdk (keyvalName, eventKeyReadKeyval)
import GI.GObject (objectNew)
import Text.PrinterParser (Color(..), toGdkColor, fromGdkColor)

-- ------------------------------------------------------------
-- * Simple Editors
-- ------------------------------------------------------------

--
-- | An invisible editor without any effect
--
noEditor :: alpha -> Editor alpha
noEditor proto =
    mkEditor
        (\ widget _ -> return ())
        (return (Just proto))

--
-- | Editor for a boolean value in the form of a check button
--
boolEditor :: Editor Bool
boolEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget bool -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- checkButtonNewWithLabel (getParameter paraName parameters)
                    setWidgetName button (getParameter paraName parameters)
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
        (paraName <<<- ParaName T.empty $ parameters)
        notifier

--
-- | Editor for a boolean value in the form of two radio buttons
----
boolEditor2 :: Text -> Editor Bool
boolEditor2 label2 parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget bool -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- vBoxNew True 2
                    radio1 <- radioButtonNewWithLabel ([]::[RadioButton]) (getParameter paraName parameters)
                    radio2 <- radioButtonNewWithLabelFromWidget (Just radio1) label2
                    boxPackStart box radio1 True True 2
                    boxPackStart box radio2 True True 2
                    widgetSetName radio1 $ getParameter paraName parameters <> ".1"
                    widgetSetName radio2 $ getParameter paraName parameters <> ".2"
                    containerAdd widget box
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
                Just (radio1,radio2) -> do
                    r <- toggleButtonGetActive radio1
                    return (Just r))
        (paraName <<<- ParaName "" $ parameters)
        notifier

--
-- | Editor for an enum value in the form of n radio buttons
----
enumEditor :: forall alpha . (Show alpha, Enum alpha, Bounded alpha)  => [Text] -> Editor alpha
enumEditor labels parameters notifier = do
    coreRef <- newIORef Nothing
    let vals :: [alpha] =  allOf
    mkEditor
        (\widget enumValue -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- vBoxNew True 2
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
                        boxPackStart box radio True True 2
                        widgetSetName radio (label <> T.pack (show n))
                        return radio) vals
                    containerAdd widget box
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
        (paraName <<<- ParaName "" $ parameters)
        notifier

-- | An Editor for nothing (which may report a click) in the form of a button
--
clickEditor :: Bool -> Editor ()
clickEditor canDefault parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget bool -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- case getParameter paraStockId parameters of
                        "" ->   buttonNewWithLabel (getParameter paraName parameters)
                        st ->   buttonNewFromStock st
                    widgetSetName button (getParameter paraName parameters)
                    containerAdd widget button
                    activateEvent button notifier Nothing Clicked
                    writeIORef coreRef (Just button)
                    when canDefault $ do
                        setWidgetCanDefault button True
                        widgetGrabDefault button
                Just button -> return ())
        (return (Just ()))
        (paraName <<<- ParaName "" $ parameters)
        notifier

-- | An Editor to display an image
--
imageEditor :: Editor Text
imageEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget stockId -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    image <- imageNewFromStock stockId (fromIntegral $ fromEnum IconSizeLargeToolbar)
                    widgetSetName image (getParameter paraName parameters)
                    containerAdd widget image
                    writeIORef coreRef (Just (image,stockId))
                Just (image,stockId2) -> imageSetFromStock image stockId (fromIntegral $ fromEnum IconSizeLargeToolbar))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_,stockId3) -> return (Just stockId3))
        parameters
        notifier

--
-- | Editor for a Text in the form of a text entry
--
textEditor :: (Text -> Bool) -> Bool -> Editor Text
textEditor validation trimBlanks parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget string -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    entry   <-  entryNew
                    widgetSetName entry (getParameter paraName parameters)
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
        parameters
        notifier

--
-- | Editor for a String in the form of a text entry
--
stringEditor :: (String -> Bool) -> Bool -> Editor String
stringEditor validation trimBlanks parameters notifier = do
    (wid,inj,ext) <- textEditor (validation . T.unpack) True parameters notifier
    return (wid, inj . T.pack, (T.unpack <$>) <$> ext)

--
-- | Editor for a multiline string in the form of a multiline text entry
--
multilineStringEditor :: Editor Text
multilineStringEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget string -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    aTextView       <-  textViewNew
                    widgetSetName aTextView (getParameter paraName parameters)
                    aScrolledWindow <-  scrolledWindowNew noAdjustment noAdjustment
                    scrolledWindowSetPolicy aScrolledWindow PolicyTypeAutomatic PolicyTypeAutomatic
                    containerAdd aScrolledWindow aTextView
                    containerAdd widget aScrolledWindow
                    mapM_ (activateEvent aTextView notifier Nothing) genericGUIEvents
                    propagateAsChanged notifier [KeyPressed]
                    buffer          <-  textViewGetBuffer aTextView
                    textBufferSetText buffer string (-1)
                    writeIORef coreRef (Just (aScrolledWindow,aTextView))
                Just (aScrolledWindow,aTextView) -> do
                    buffer          <-  textViewGetBuffer aTextView
                    textBufferSetText buffer string (-1))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (aScrolledWindow, aTextView) -> do
                    buffer          <-  textViewGetBuffer aTextView
                    start           <-  textBufferGetStartIter buffer
                    end             <-  textBufferGetEndIter buffer
                    r               <-  textBufferGetText buffer start end False
                    return (Just r))
        parameters
        notifier

--
-- | Editor for an integer in the form of a spin entry
--
intEditor :: (Double,Double,Double) -> Editor Int
intEditor (min, max, step) parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget v -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    spin <- spinButtonNewWithRange min max step
                    widgetSetName spin (getParameter paraName parameters)
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
        parameters
        notifier

--
-- | Editor for for any value which is an instance of Read and Show in the form of a
-- | text entry
genericEditor :: (Show beta, Read beta) => Editor beta
genericEditor parameters notifier = do
    (wid,inj,ext) <- stringEditor (const True) True parameters notifier
    let ginj = inj . show
    let gext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> E.catch (liftM Just (readIO s))
                            (\(e :: IOException) -> do
                                putStrLn ("Generic editor no parse for " ++ s ++ " " ++ show e)
                                return Nothing)
    return (wid,ginj,gext)

--
-- | Editor for no value, it only emtis a clicked event and has the form of a check button
--
buttonEditor :: Editor ()
buttonEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget _ -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel (getParameter paraName parameters)
                    widgetSetName button (getParameter paraName parameters)
                    containerAdd widget button
                    mapM_ (activateEvent button notifier Nothing) (Clicked:genericGUIEvents)
                    writeIORef coreRef (Just button)
                Just button -> return ())
        (return (Just ()))
        parameters
        notifier

--
-- | Editor for the selection of some element from a static list of elements in the
-- | form of a combo box
comboSelectionEditor :: Eq beta => [beta] -> (beta -> Text) -> Editor beta
comboSelectionEditor list showF parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget obj -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    combo <- comboBoxNewText
                    widgetSetSizeRequest combo 200 (-1)
                    mapM_ (comboBoxAppendText combo . showF) list
                    widgetSetName combo (getParameter paraName parameters)
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
                    case ind of
                        (-1)   -> return Nothing
                        otherwise  -> return (Just (list !! fromIntegral ind)))
        parameters
        notifier

-- | Like comboSelectionEditor but allows entry of text not in the list
comboEntryEditor :: [Text] -> Editor Text
comboEntryEditor list parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget obj -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    combo <- comboBoxNewWithEntry
                    comboBoxSetModelText combo
                    widgetSetSizeRequest combo 200 (-1)
                    mapM_ (comboBoxAppendText combo) list
                    widgetSetName combo (getParameter paraName parameters)
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
                        Nothing -> do
                            entry <- binGetChild combo >>= unsafeCastTo Entry
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
                    entry <- binGetChild combo >>= unsafeCastTo Entry
                    Just <$> entryGetText entry)
        parameters
        notifier

--
-- | Editor for the selection of some elements from a list of elements in the
-- | form of a list box
multiselectionEditor :: (Show beta, Eq beta) => Editor [beta]
multiselectionEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget objs -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    seqStore   <- seqStoreNew []
                    listView    <- treeViewNewWithModel seqStore
                    widgetSetName listView (getParameter paraName parameters)
                    mapM_ (activateEvent listView notifier Nothing) genericGUIEvents
                    propagateAsChanged notifier [KeyPressed,ButtonPressed]
                    sel         <- treeViewGetSelection listView
                    treeSelectionSetMode sel SelectionModeMultiple
                    renderer    <- cellRendererTextNew
                    col         <- treeViewColumnNew
                    treeViewAppendColumn listView col
                    cellLayoutPackStart col renderer True
                    cellLayoutSetAttributeFunc col renderer seqStore (
                        customStoreGetRow seqStore >=> setCellRendererTextText renderer . T.pack . show)
                    treeViewSetHeadersVisible listView False
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore) objs
                    containerAdd widget listView
                    treeSelectionUnselectAll sel
                    --let inds = catMaybes $map (\obj -> elemIndex obj list) objs
                    --mapM_ (\i -> treeSelectionSelectPath sel [i]) inds
                    writeIORef coreRef (Just (listView,seqStore))
                Just (listView,seqStore) -> do
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
        parameters
        notifier

--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box with toggle elements

staticListMultiEditor :: (Eq beta) => [beta] -> (beta -> Text) -> Editor [beta]
staticListMultiEditor list showF parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget objs -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    seqStore <- seqStoreNew ([]:: [(Bool,beta)])
                    listView <- treeViewNewWithModel seqStore
                    widgetSetName listView (getParameter paraName parameters)
                    mapM_ (activateEvent listView notifier Nothing) genericGUIEvents
                    propagateAsChanged notifier [KeyPressed,ButtonPressed]
                    sel <- treeViewGetSelection listView
                    treeSelectionSetMode sel SelectionModeSingle
                    rendererToggle <- cellRendererToggleNew
                    setCellRendererToggleActivatable rendererToggle True
                    rendererText <- cellRendererTextNew
                    col1 <- treeViewColumnNew
                    treeViewAppendColumn listView col1
                    cellLayoutPackStart col1 rendererToggle True
                    cellLayoutSetAttributeFunc col1 rendererToggle seqStore (
                        customStoreGetRow seqStore >=> setCellRendererToggleActive rendererToggle . fst)
                    col2 <- treeViewColumnNew
                    treeViewAppendColumn listView col2
                    cellLayoutPackStart col2 rendererText True
                    cellLayoutSetAttributeFunc col2 rendererText seqStore (
                        customStoreGetRow seqStore >=> setCellRendererTextText rendererText . showF . snd)
                    treeViewSetHeadersVisible listView False
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore . (\ e -> (e `elem` objs, e))) list
                    let minSize =   getParameter paraMinSize parameters
                    uncurry (widgetSetSizeRequest listView) minSize
                    sw <- scrolledWindowNew noAdjustment noAdjustment
                    containerAdd sw listView
                    scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic
                    containerAdd widget sw
                    scrolledWindowSetMinContentHeight sw (snd minSize)
                      -- update the model when the toggle buttons are activated
                    onCellRendererToggleToggled rendererToggle $ \pathStr -> do
                        let (i:_) = stringToTreePath pathStr
                        val <- seqStoreGetValue seqStore i
                        seqStoreSetValue seqStore i (not (fst val),snd val)
                    onWidgetKeyPressEvent listView $ \e -> do
                        name <- eventKeyReadKeyval e >>= keyvalName
                        liftIO $
                            case name of
                                "Return" -> do
                                    sel <- treeViewGetSelection listView
                                    rows <- treeSelectionGetSelectedRows' sel >>= mapM treePathGetIndices'
                                    mapM_ (\ (i:_) -> do
                                        val <- seqStoreGetValue seqStore i
                                        seqStoreSetValue seqStore i (not (fst val),snd val)) rows
                                    return True
                                _ -> return False
                    writeIORef coreRef (Just (listView,seqStore))
                Just (listView,seqStore) -> do
                    let model = map (\e -> (e `elem` objs,e)) list
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore . (\ e -> (e `elem` objs, e))) list)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (listView,seqStore) -> do
                    model <- seqStoreToList seqStore
                    return (Just (map snd $ filter fst model)))
        parameters
        notifier

--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box

staticListEditor :: (Eq beta) => [beta] -> (beta -> Text) -> Editor beta
staticListEditor list showF parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget obj -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    seqStore <- seqStoreNew ([]:: [alpha])
                    listView <- treeViewNewWithModel seqStore
                    widgetSetName listView (getParameter paraName parameters)
                    mapM_ (activateEvent listView notifier Nothing) genericGUIEvents
                    propagateAsChanged notifier [KeyPressed,ButtonPressed]
                    sel <- treeViewGetSelection listView
                    treeSelectionSetMode sel
                        (if getParameter paraMultiSel parameters
                            then SelectionModeMultiple
                            else SelectionModeSingle)
                    renderer <- cellRendererTextNew
                    col <- treeViewColumnNew
                    treeViewAppendColumn listView col
                    cellLayoutPackStart col renderer True
                    cellLayoutSetAttributeFunc col renderer seqStore (
                        customStoreGetRow seqStore >=> (setCellRendererTextText renderer . showF))
                    treeViewSetHeadersVisible listView False
                    seqStoreClear seqStore
                    mapM_ (seqStoreAppend seqStore) list
                    let minSize =   getParameter paraMinSize parameters
                    uncurry (widgetSetSizeRequest listView) minSize
                    sw <- scrolledWindowNew noAdjustment noAdjustment
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
        parameters
        notifier


--
-- | Editor for the selection of a file path in the form of a text entry and a button,
-- | which opens a gtk file chooser
fileEditor :: Maybe FilePath -> FileChooserAction -> Text -> Editor FilePath
fileEditor mbFilePath action buttonName parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget filePath -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel buttonName
                    widgetSetName button $ getParameter paraName parameters <> "-button"
                    mapM_ (activateEvent button notifier Nothing)
                        (Clicked:genericGUIEvents)
                    entry <- entryNew
                    widgetSetName entry $ getParameter paraName parameters <> "-entry"
                    -- set entry [ entryEditable := False ]
                    mapM_ (activateEvent entry notifier Nothing) genericGUIEvents
                    registerEvent notifier Clicked (buttonHandler entry)
                    propagateAsChanged notifier [KeyPressed,ButtonPressed]
                    box <- case getParameter paraDirection parameters of
                                Horizontal  -> do
                                    r <- hBoxNew False 1
                                    toBox r
                                Vertical    -> do
                                    r <- vBoxNew False 1
                                    toBox r
                    boxPackStart box entry True True 0
                    boxPackEnd box button False False 0
                    containerAdd widget box
                    entrySetText entry (T.pack filePath)
                    writeIORef coreRef (Just entry)
                Just entry -> entrySetText entry (T.pack filePath))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just entry -> do
                    str <- entryGetText entry
                    return (Just (T.unpack str)))
        parameters
        notifier
    where
    buttonHandler entry e =  do
        mbFileName <- do
            dialog <- new FileChooserDialog []
            setWindowTitle dialog "Select File"
            dialogAddButton dialog "gtk-cancel" (fromIntegral $ fromEnum ResponseTypeCancel)
            dialogAddButton dialog "gtk-open" (fromIntegral $ fromEnum ResponseTypeAccept)
            fileChooserSetAction dialog action
            widgetShow dialog
            response <- dialogRun dialog
            case toEnum $ fromIntegral response of
                ResponseTypeAccept -> do
                    f <- fileChooserGetFilename dialog
                    widgetDestroy dialog
                    return $ Just f
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
                triggerEvent notifier GUIEvent {
                    selector = MayHaveChanged,
                    eventText = "",
                    gtkReturn = True}
                return (e{gtkReturn=True})

--
-- | Editor for a font selection
--
fontEditor :: Editor (Maybe Text)
fontEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget mbValue -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    fs <- fontButtonNew
                    widgetSetName fs $ getParameter paraName parameters
                    mapM_ (activateEvent fs notifier Nothing) (Clicked: genericGUIEvents)
                    activateEvent fs notifier
                        (Just (\ w h -> do
                            res <- onFontButtonFontSet w (void h)
                            return (unsafeCoerce res))) MayHaveChanged
                    containerAdd widget fs
                    case mbValue of
                        Nothing -> return True
                        Just s  -> fontButtonSetFontName fs s
                    writeIORef coreRef (Just fs)
                Just fs ->   case mbValue of
                                Nothing -> return ()
                                Just s  -> do
                                    fontButtonSetFontName fs s
                                    return ())
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just fs -> do
                    f <- fontButtonGetFontName fs
                    return (Just (Just f)))
        parameters
        notifier

--
-- | Editor for color selection
--
colorEditor :: Editor Color
colorEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget c -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    cs <- colorButtonNew
                    widgetSetName cs $ getParameter paraName parameters
                    mapM_ (activateEvent cs notifier Nothing) (Clicked: genericGUIEvents)
                    activateEvent cs notifier
                        (Just (\ w h -> do
                            res <- onColorButtonColorSet w (void h)
                            return (unsafeCoerce res))) MayHaveChanged
                    containerAdd widget cs
                    colorButtonSetColor cs =<< toGdkColor c
                    writeIORef coreRef (Just cs)
                Just cs -> colorButtonSetColor cs =<< toGdkColor c)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just cs -> do
                    c <- colorButtonGetColor cs >>= fromGdkColor
                    return (Just c))
        parameters
        notifier

--
-- | An editor, which opens another editor
--   You have to inject a value before the button can be clicked.
--
otherEditor :: (alpha  -> Text -> IO (Maybe alpha)) -> Editor alpha
otherEditor func parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget val -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel (getParameter paraName parameters)
                    widgetSetName button $ getParameter paraName parameters
                    containerAdd widget button
                    mapM_ (activateEvent button notifier Nothing) (Clicked:genericGUIEvents)
                    registerEvent notifier Clicked (buttonHandler coreRef)
                    propagateAsChanged notifier [KeyPressed,ButtonPressed,Clicked]
                    writeIORef coreRef (Just (button,val))
                Just (button, oldval) -> writeIORef coreRef (Just (button, val)))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_,val) -> return (Just val))
        (paraName <<<- ParaName "" $ parameters)
        notifier
    where
    buttonHandler coreRef e = do
        core <- readIORef coreRef
        case core of
            Nothing -> error "You have to inject a value before the button can be clicked"
            Just (b,val) -> do
                res <- func val (getParameter paraName parameters)
                case res of
                    Nothing     -> return (e{gtkReturn=True})
                    Just nval   -> do
                        writeIORef coreRef (Just (b, nval))
                        return (e{gtkReturn=True})

okCancelFields :: FieldDescription ()
okCancelFields = HFD emptyParams [
        mkField
            (paraStockId <<<- ParaStockId STOCK_CANCEL
                $ paraName <<<- ParaName "Cancel"
                    $ emptyParams)
            (const ())
            (\ _ b -> b)
            (clickEditor False)
    ,   mkField
            (paraStockId <<<- ParaStockId STOCK_OK
                $ paraName <<<- ParaName "Ok"
                    $ emptyParams)
            (const ())
            (\ a b -> b)
            (clickEditor True)]

