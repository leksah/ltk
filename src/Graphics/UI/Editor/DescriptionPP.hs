{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.DescriptionPP
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Description of a editor with additional fileds for printing and parsing
--
-----------------------------------------------------------------------------------
module Graphics.UI.Editor.DescriptionPP (
    Applicator
,   FieldDescriptionPP(..)
,   mkFieldPP
,   extractFieldDescription
,   flattenFieldDescriptionPP
) where

import Control.Monad
import Data.Aeson (FromJSON(..), ToJSON(..))

import Graphics.UI.Editor.Parameters
import Graphics.UI.Editor.MakeEditor
--import IDE.Core.State
import Graphics.UI.Editor.Basics (Applicator(..),Editor(..),Setter(..),Getter(..),Notifier(..),Extractor(..),Injector(..))
import qualified Data.Text as T (unpack)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Control.Arrow as A (Arrow(..))
import GI.Gtk.Objects.Widget (Widget(..))

data FieldDescriptionPP alpha gamma =  FDPP {
        parameters      ::  Parameters
    ,   fieldEditor     ::  alpha -> IO (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
    ,   applicator      ::  alpha -> alpha -> gamma ()}
    | VFDPP Parameters [FieldDescriptionPP alpha gamma]
    | HFDPP Parameters [FieldDescriptionPP alpha gamma]
    | NFDPP [(Text,FieldDescriptionPP alpha gamma)]

type MkFieldDescriptionPP alpha beta gamma =
    Parameters      ->
    Getter alpha beta      ->
    Setter alpha beta      ->
    Editor beta        ->
    Applicator beta gamma     ->
    FieldDescriptionPP alpha gamma

mkFieldPP :: (Eq beta, Monad gamma) => MkFieldDescriptionPP alpha beta gamma
mkFieldPP parameters getter setter editor applicator  =
    let FD _ ed = mkField parameters getter setter editor
    in FDPP parameters
        ed
        (\ newDat oldDat -> do --applicator
            let newField = getter newDat
            let oldField = getter oldDat
            unless (newField == oldField) $ applicator newField)

extractFieldDescription :: FieldDescriptionPP alpha gamma -> FieldDescription alpha
extractFieldDescription (VFDPP paras descrs) =  VFD paras (map extractFieldDescription descrs)
extractFieldDescription (HFDPP paras descrs) =  HFD paras (map extractFieldDescription descrs)
extractFieldDescription (NFDPP descrsp)      =  NFD (map (A.second extractFieldDescription) descrsp)
extractFieldDescription (FDPP parameters fieldEditor applicator) =
    FD parameters fieldEditor

flattenFieldDescriptionPP :: FieldDescriptionPP alpha gamma -> [FieldDescriptionPP alpha gamma]
flattenFieldDescriptionPP (VFDPP paras descrs)  =   concatMap flattenFieldDescriptionPP descrs
flattenFieldDescriptionPP (HFDPP paras descrs)  =   concatMap flattenFieldDescriptionPP descrs
flattenFieldDescriptionPP (NFDPP descrsp)       =   concatMap (flattenFieldDescriptionPP . snd) descrsp
flattenFieldDescriptionPP fdpp                  =   [fdpp]

