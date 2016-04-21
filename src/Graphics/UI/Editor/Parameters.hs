{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.Parameters
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for parameters for editors
--
-----------------------------------------------------------------------------------

module Graphics.UI.Editor.Parameters (
    Parameters
,   Parameter(..)
,   paraName
,   paraSynopsis
,   paraDirection
,   paraShowLabel
,   paraShadow
,   paraOuterAlignment
,   paraInnerAlignment
,   paraOuterPadding
,   paraInnerPadding
,   paraMinSize
,   paraHorizontal
,   paraStockId
,   paraMultiSel
,   paraPack

,   getParameter
,   getParameterPrim
,   (<<<-)
,   emptyParams
,   Packing(..)
,   boxPackStart'
,   boxPackEnd'
,   dialogAddButton'
,   dialogSetDefaultResponse'
,   dialogResponse'
,   dialogRun'
,   Direction(..)
,   HorizontalAlign(..)
) where

import Prelude ()
import Prelude.Compat
import Data.Maybe
import Data.Text (Text)
import qualified Data.List as List
import GI.Gtk.Enums (ResponseType, ShadowType(..))
import GI.Gtk.Objects.Box (boxPackStart, BoxK, boxPackEnd)
import Control.Monad.IO.Class (MonadIO)
import GI.Gtk.Objects.Widget (Widget(..), WidgetK)
import Data.Word (Word32)
import Data.Int (Int32)
import qualified Data.Text as T (Text)
import GI.Gtk.Objects.Dialog
       (dialogResponse, dialogSetDefaultResponse, DialogK, dialogRun,
        dialogAddButton)
import GI.Gtk.Structs.TreePath
       (treePathNew, TreePath(..))

data Packing = PackRepel | PackGrow | PackNatural deriving (Eq, Show)

boxPackStart' :: (MonadIO m, BoxK a, WidgetK b) => a -> b -> Packing -> Word32 -> m ()
boxPackStart' a b PackRepel   = boxPackStart a b True  False
boxPackStart' a b PackGrow    = boxPackStart a b True  True
boxPackStart' a b PackNatural = boxPackStart a b False False

boxPackEnd' :: (MonadIO m, BoxK a, WidgetK b) => a -> b -> Packing -> Word32 -> m ()
boxPackEnd' a b PackRepel   = boxPackEnd a b True  False
boxPackEnd' a b PackGrow    = boxPackEnd a b True  True
boxPackEnd' a b PackNatural = boxPackEnd a b False False

dialogAddButton' :: (MonadIO m, DialogK d) => d -> T.Text -> ResponseType -> m Widget
dialogAddButton' d t r = dialogAddButton d t (fromIntegral $ fromEnum r)

dialogSetDefaultResponse' :: (MonadIO m, DialogK d) => d -> ResponseType -> m ()
dialogSetDefaultResponse' d r = dialogSetDefaultResponse d (fromIntegral $ fromEnum r)

dialogResponse' :: (MonadIO m, DialogK d) => d -> ResponseType -> m ()
dialogResponse' d r = dialogResponse d (fromIntegral $ fromEnum r)

dialogRun' :: (Applicative m, MonadIO m, DialogK d) => d -> m ResponseType
dialogRun' d = toEnum . fromIntegral <$> dialogRun d

--
-- | The direction of a split
--
data Direction      =   Horizontal | Vertical
    deriving (Eq,Show)

data HorizontalAlign =   StartHorizontal | StopHorizontal | Keep
    deriving (Eq,Show)
--
-- | A type for parameters for editors
--
type Parameters     =   [Parameter]

data Parameter      =   ParaName Text
                    |   ParaSynopsis Text
                    |   ParaDirection Direction
                    |   ParaShadow ShadowType
                    |   ParaShowLabel Bool
                    |   ParaOuterAlignment  (Float,Float,Float,Float)
                                               -- | xalign yalign xscale yscale
                    |   ParaOuterPadding    (Word32,Word32,Word32,Word32)
                                                --  | paddingTop paddingBottom paddingLeft paddingRight
                    |   ParaInnerAlignment  (Float,Float,Float,Float)
                                                -- | xalign yalign xscale yscale
                    |   ParaInnerPadding   (Word32,Word32,Word32,Word32)
                                                --  | paddingTop paddingBottom paddingLeft paddingRight
                    |   ParaMinSize         (Int32, Int32)
                    |   ParaHorizontal      HorizontalAlign
                    |   ParaStockId Text
                    |   ParaMultiSel Bool
                    |   ParaPack Packing
    deriving (Eq,Show)


emptyParams         ::   [Parameter]
emptyParams         =   []

paraName                        ::   Parameter -> Maybe Text
paraName (ParaName str)         =   Just str
paraName _                      =   Nothing

paraSynopsis                    ::   Parameter -> Maybe Text
paraSynopsis (ParaSynopsis str) =   Just str
paraSynopsis _                  =   Nothing

paraShowLabel                    ::   Parameter -> Maybe Bool
paraShowLabel (ParaShowLabel b)  =   Just b
paraShowLabel _                  =   Nothing

paraDirection                   ::   Parameter -> Maybe Direction
paraDirection (ParaDirection d) =   Just d
paraDirection _                 =   Nothing

paraShadow                      ::   Parameter -> Maybe ShadowType
paraShadow (ParaShadow d)       =   Just d
paraShadow _                    =   Nothing

paraOuterAlignment              ::   Parameter -> Maybe (Float,Float,Float,Float)
paraOuterAlignment (ParaOuterAlignment d) = Just d
paraOuterAlignment _            =   Nothing

paraInnerAlignment              ::   Parameter -> Maybe (Float,Float,Float,Float)
paraInnerAlignment (ParaInnerAlignment d) = Just d
paraInnerAlignment _            =   Nothing

paraOuterPadding                ::   Parameter -> Maybe (Word32,Word32,Word32,Word32)
paraOuterPadding (ParaOuterPadding d) = Just d
paraOuterPadding _              =   Nothing

paraInnerPadding                ::   Parameter -> Maybe (Word32,Word32,Word32,Word32)
paraInnerPadding (ParaInnerPadding d) = Just d
paraInnerPadding _              =   Nothing

paraMinSize                     ::   Parameter -> Maybe (Int32, Int32)
paraMinSize (ParaMinSize d)     =   Just d
paraMinSize _                   =   Nothing

paraHorizontal                  ::   Parameter -> Maybe HorizontalAlign
paraHorizontal (ParaHorizontal d) =   Just d
paraHorizontal _                =   Nothing

paraStockId                     ::   Parameter -> Maybe Text
paraStockId (ParaStockId str)   =   Just str
paraStockId _                   =   Nothing

paraMultiSel                    ::   Parameter -> Maybe Bool
paraMultiSel (ParaMultiSel b)   =   Just b
paraMultiSel _                  =   Nothing

paraPack                        ::   Parameter -> Maybe Packing
paraPack (ParaPack b)           =   Just b
paraPack _                      =   Nothing

--
-- | Convenience method to get a parameter, or if not set the default parameter
--
getParameter :: (Parameter -> Maybe beta) -> Parameters -> beta
getParameter selector parameter =
    case getParameterPrim selector parameter of
        Just ele       -> ele
        _              -> case getParameterPrim selector defaultParameters of
                            Just ele       -> ele
                            _              -> error "default parameter not defined"

getParameterPrim :: (Parameter -> Maybe beta) -> Parameters -> Maybe beta
getParameterPrim selector parameter =
    case filter isJust $ map selector parameter of
        Just ele : _ -> Just ele
        _            -> Nothing

(<<<-) :: (Parameter -> Maybe beta) -> Parameter -> Parameters -> Parameters
(<<<-) selector para  params = para : filter (isNothing . selector) params

defaultParameters :: Parameters
defaultParameters =
    [   ParaName ""
    ,   ParaStockId ""
    ,   ParaSynopsis ""
    ,   ParaDirection Horizontal
    ,   ParaShadow ShadowTypeNone
    ,   ParaOuterAlignment  (0.4, 0.5, 1.0, 0.7)
    ,   ParaOuterPadding    (5, 5, 5, 5)
    ,   ParaInnerAlignment  (0.4, 0.5, 1.0, 0.7)
    ,   ParaInnerPadding    (5, 5, 5, 5)
    ,   ParaMinSize         (-1,-1)
    ,   ParaHorizontal      Keep
    ,   ParaMultiSel True
    ,   ParaPack PackNatural
    ,   ParaShowLabel True
    ]

