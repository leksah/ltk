{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
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
,   paraOrientation
,   paraShowLabel
,   paraShadow
,   paraMargin
,   paraMinSize
,   paraHAlign
,   paraVAlign
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
) where

import Prelude ()
import Prelude.Compat
import Data.Maybe
import Data.Text (Text)
import GI.Gtk.Enums (Orientation(..), ResponseType, ShadowType(..))
import GI.Gtk.Objects.Box (boxPackStart, IsBox, boxPackEnd)
import Control.Monad.IO.Class (MonadIO)
import GI.Gtk.Objects.Widget (Widget(..), IsWidget)
import Data.Word (Word32)
import Data.Int (Int32)
import qualified Data.Text as T (Text)
import GI.Gtk.Objects.Dialog
       (dialogResponse, dialogSetDefaultResponse, IsDialog, dialogRun,
        dialogAddButton)
import GI.Gtk (Align(..))

data Packing = PackRepel | PackGrow | PackNatural deriving (Eq, Show)

boxPackStart' :: (MonadIO m, IsBox a, IsWidget b) => a -> b -> Packing -> Word32 -> m ()
boxPackStart' a b PackRepel   = boxPackStart a b True  False
boxPackStart' a b PackGrow    = boxPackStart a b True  True
boxPackStart' a b PackNatural = boxPackStart a b False False

boxPackEnd' :: (MonadIO m, IsBox a, IsWidget b) => a -> b -> Packing -> Word32 -> m ()
boxPackEnd' a b PackRepel   = boxPackEnd a b True  False
boxPackEnd' a b PackGrow    = boxPackEnd a b True  True
boxPackEnd' a b PackNatural = boxPackEnd a b False False

dialogAddButton' :: (MonadIO m, IsDialog d) => d -> T.Text -> ResponseType -> m Widget
dialogAddButton' d t r = dialogAddButton d t (fromIntegral $ fromEnum r)

dialogSetDefaultResponse' :: (MonadIO m, IsDialog d) => d -> ResponseType -> m ()
dialogSetDefaultResponse' d r = dialogSetDefaultResponse d (fromIntegral $ fromEnum r)

dialogResponse' :: (MonadIO m, IsDialog d) => d -> ResponseType -> m ()
dialogResponse' d r = dialogResponse d (fromIntegral $ fromEnum r)

dialogRun' :: (Applicative m, MonadIO m, IsDialog d) => d -> m ResponseType
dialogRun' d = toEnum . fromIntegral <$> dialogRun d

data HorizontalAlign =   StartHorizontal | StopHorizontal | Keep
    deriving (Eq,Show)
--
-- | A type for parameters for editors
--
type Parameters     =   [Parameter]

data Parameter      =   ParaName Text
                    |   ParaSynopsis Text
                    |   ParaOrientation Orientation
                    |   ParaShadow ShadowType
                    |   ParaShowLabel Bool
                    |   ParaMargin    (Int32,Int32,Int32,Int32)
                                      -- ^ marginTop marginBottom marginLeft marginRight
                    |   ParaMinSize    (Int32, Int32)
                    |   ParaHAlign Align
                    |   ParaVAlign Align
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

paraOrientation                 ::   Parameter -> Maybe Orientation
paraOrientation (ParaOrientation d) =   Just d
paraOrientation _               =   Nothing

paraShadow                      ::   Parameter -> Maybe ShadowType
paraShadow (ParaShadow d)       =   Just d
paraShadow _                    =   Nothing

paraMargin                      ::   Parameter -> Maybe (Int32,Int32,Int32,Int32)
paraMargin (ParaMargin d)       =   Just d
paraMargin _                    =   Nothing

paraMinSize                     ::   Parameter -> Maybe (Int32, Int32)
paraMinSize (ParaMinSize d)     =   Just d
paraMinSize _                   =   Nothing

paraHAlign                      ::   Parameter -> Maybe Align
paraHAlign (ParaHAlign d)       =   Just d
paraHAlign _                    =   Nothing

paraVAlign                      ::   Parameter -> Maybe Align
paraVAlign (ParaVAlign d)       =   Just d
paraVAlign _                    =   Nothing

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
    ,   ParaSynopsis ""
    ,   ParaOrientation OrientationHorizontal
    ,   ParaShadow ShadowTypeNone
    ,   ParaMargin  (5, 5, 5, 5)
    ,   ParaMinSize (-1,-1)
    ,   ParaHAlign  AlignFill
    ,   ParaVAlign  AlignFill
    ,   ParaMultiSel True
    ,   ParaPack PackNatural
    ,   ParaShowLabel True
    ]

