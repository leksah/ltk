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
,   flattenFieldDescriptionPPToS
) where

import Graphics.UI.Gtk
import Control.Monad
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.ParserCombinators.Parsec as P

import Text.PrinterParser hiding (fieldParser,parameters)
import Graphics.UI.Editor.Parameters
import Graphics.UI.Editor.MakeEditor
--import IDE.Core.State
import Graphics.UI.Editor.Basics (Applicator(..),Editor(..),Setter(..),Getter(..),Notifier(..),Extractor(..),Injector(..))

data FieldDescriptionPP alpha gamma =  FDPP {
        parameters      ::  Parameters
    ,   fieldPrinter    ::  alpha -> PP.Doc
    ,   fieldParser     ::  alpha -> P.CharParser () alpha
    ,   fieldEditor     ::  alpha -> IO (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
    ,   applicator      ::  alpha -> alpha -> gamma ()
    }
    | VFDPP Parameters [FieldDescriptionPP alpha gamma]
    | HFDPP Parameters [FieldDescriptionPP alpha gamma]
    | NFDPP [(String,FieldDescriptionPP alpha gamma)]

type MkFieldDescriptionPP alpha beta gamma =
    Parameters      ->
    (Printer beta)     ->
    (Parser beta)      ->
    (Getter alpha beta)    ->
    (Setter alpha beta)    ->
    (Editor beta)      ->
    (Applicator beta gamma )  ->
    FieldDescriptionPP alpha gamma

mkFieldPP :: (Eq beta, Monad gamma) => MkFieldDescriptionPP alpha beta gamma
mkFieldPP parameters printer parser getter setter editor applicator  =
    let FD _ ed = mkField parameters getter setter editor
    in FDPP parameters
        (\ dat -> (PP.text (case getParameterPrim paraName parameters of
                                    Nothing -> ""
                                    Just str -> str) PP.<> PP.colon)
                PP.$$ (PP.nest 15 (printer (getter dat)))
                PP.$$ (PP.nest 5 (case getParameterPrim paraSynopsis parameters of
                                    Nothing -> PP.empty
                                    Just str -> PP.text $"--" ++ str)))
        (\ dat -> P.try (do
            symbol (case getParameterPrim paraName parameters of
                                    Nothing -> ""
                                    Just str -> str)
            colon
            val <- parser
            return (setter val dat)))
        ed
        (\ newDat oldDat -> do --applicator
            let newField = getter newDat
            let oldField = getter oldDat
            if newField == oldField
                then return ()
                else applicator newField)

extractFieldDescription :: FieldDescriptionPP alpha gamma -> FieldDescription alpha
extractFieldDescription (VFDPP paras descrs) =  VFD paras (map extractFieldDescription descrs)
extractFieldDescription (HFDPP paras descrs) =  HFD paras (map extractFieldDescription descrs)
extractFieldDescription (NFDPP descrsp)      =  NFD (map (\(s,d) ->
                                                    (s, extractFieldDescription d)) descrsp)
extractFieldDescription (FDPP parameters fieldPrinter fieldParser fieldEditor applicator) =
    (FD parameters fieldEditor)

flattenFieldDescriptionPP :: FieldDescriptionPP alpha gamma -> [FieldDescriptionPP alpha gamma]
flattenFieldDescriptionPP (VFDPP paras descrs)  =   concatMap flattenFieldDescriptionPP descrs
flattenFieldDescriptionPP (HFDPP paras descrs)  =   concatMap flattenFieldDescriptionPP descrs
flattenFieldDescriptionPP (NFDPP descrsp)       =   concatMap (flattenFieldDescriptionPP . snd) descrsp
flattenFieldDescriptionPP fdpp                  =   [fdpp]

flattenFieldDescriptionPPToS :: FieldDescriptionPP alpha gamma -> [FieldDescriptionS alpha]
flattenFieldDescriptionPPToS = map ppToS . flattenFieldDescriptionPP

ppToS :: FieldDescriptionPP alpha gamma -> FieldDescriptionS alpha
ppToS (FDPP para print pars _ _) = FDS para print pars
ppToS _                          = error "DescriptionPP.ppToS Can't transform"

