{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}
--
-- | Module for saving and restoring preferences and settings
--

module Text.PrinterParser (

    Printer
,   Parser
,   FieldDescriptionS(..)
,   MkFieldDescriptionS
,   mkFieldS

,   applyFieldParsers
,   boolParser
,   intParser
,   lineParser
,   pairParser
,   identifier
,   emptyParser
,   whiteSpace
,   stringParser
,   readParser
,   colorParser

,   emptyPrinter
,   Pretty(..)
,   prettyPrint
,   maybePP

,   symbol
,   colon

,   writeFields
,   showFields
,   readFields
,   parseFields
) where

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec hiding(Parser)
import qualified Text.PrettyPrint as PP

import Graphics.UI.Editor.Parameters
import Graphics.UI.Editor.Basics
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Graphics.UI.Gtk (Color(..))
import Data.List (foldl')
import qualified Text.ParserCombinators.Parsec as  P
    ((<?>), CharParser(..), parseFromFile)
import Control.Exception as E (catch, IOException)
import qualified Data.Text as T (pack, unpack)
import Data.Text (Text)
import Control.Applicative ((<$>))


type Printer beta       =   beta -> PP.Doc
type Parser beta        =   CharParser () beta

-- ------------------------------------------------------------
-- * Parsing with Parsec
-- ------------------------------------------------------------

data FieldDescriptionS alpha =  FDS {
        parameters      ::  Parameters
    ,   fieldPrinter    ::  alpha -> PP.Doc
    ,   fieldParser     ::  alpha -> CharParser () alpha
    }

type MkFieldDescriptionS alpha beta =
    Parameters ->
    Printer beta ->
    Parser beta ->
    Getter alpha beta ->
    Setter alpha beta ->
    FieldDescriptionS alpha

mkFieldS :: {--Eq beta =>--} MkFieldDescriptionS alpha beta
mkFieldS parameter printer parser getter setter =
    FDS parameter
        (\ dat -> (PP.text (case getParameterPrim paraName parameter of
                                Nothing -> ""
                                Just str -> T.unpack str) PP.<> PP.colon)
                PP.$$ PP.nest 15 (printer (getter dat))
                PP.$$ PP.nest 5 (case getParameterPrim paraSynopsis parameter of
                                    Nothing -> PP.empty
                                    Just str -> PP.text . T.unpack $ "--" <> str))
        (\ dat -> try (do
            symbol (fromMaybe "" (getParameterPrim paraName parameter))
            colon
            val <- parser
            return (setter val dat)))

applyFieldParsers ::  a ->  [a ->  CharParser () a] ->  CharParser () a
applyFieldParsers prefs parseF = do
    eof
    return prefs
    <|> do
    let parsers = map (\a ->  a prefs) parseF
    newprefs <-  choice parsers
    whiteSpace
    applyFieldParsers newprefs parseF
    <?> "field parser"

boolParser ::  CharParser () Bool
boolParser = do
    symbol "True" <|> symbol "true"
    return True
    <|> do
    symbol "False" <|> symbol "false"
    return False
    <?> "bool parser"


readParser ::  Read a =>  CharParser () a
readParser = do
    str <- many (noneOf "\n")
    if null str
        then unexpected "read parser on empty string"
        else
            case maybeRead str of
                Nothing -> unexpected $ "read parser no parse " ++ str
                Just r -> return r
    <?> "read parser"
        where maybeRead = listToMaybe . map fst . filter (null . snd) . reads

pairParser ::  CharParser () alpha ->  CharParser () (alpha,alpha)
pairParser p2 = do
    char '('
    v1 <-  p2
    char ','
    v2 <-  p2
    char ')'
    return (v1,v2)
    <?> "pair parser"

stringParser ::  CharParser () Text
stringParser = do
    char '"'
    str <- many (noneOf "\"")
    char '"'
    return (T.pack str)
    <?> "string parser"

lineParser ::  CharParser () Text
lineParser = do
    str <- many (noneOf "\n")
    return (T.pack str)
    <?> "line parser"


intParser ::  CharParser () Int
intParser = do
    i <-  integer
    return (fromIntegral i)

colorParser :: CharParser () Color
colorParser = do
    string "Color"
    whiteSpace
    r <- integer
    whiteSpace
    g <- integer
    whiteSpace
    b <- integer
    return $ Color (fromIntegral r) (fromIntegral g) (fromIntegral b)

emptyParser ::  CharParser () ()
emptyParser = pzero

prefsStyle  ::  P.LanguageDef st
prefsStyle  = emptyDef  {
        P.commentStart   = "{-"
    ,   P.commentEnd     = "-}"
    ,   P.commentLine    = "--"
    }

lexer :: P.TokenParser st
lexer = P.makeTokenParser prefsStyle

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

symbol :: Text -> CharParser st Text
symbol = (T.pack <$>) . P.symbol lexer . T.unpack

identifier, colon :: CharParser st Text
identifier = T.pack <$> P.identifier lexer
colon = T.pack <$> P.colon lexer

integer = P.integer lexer

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------
-- | pretty-print with the default style and 'defaultMode'.
prettyPrint :: Pretty a => a -> Text
prettyPrint a = T.pack $ PP.renderStyle  PP.style (pretty a)

-- | Things that can be pretty-printed
class Pretty a where
	-- | Pretty-print something in isolation.
	pretty :: a -> PP.Doc
	-- | Pretty-print something in a precedence context.
	prettyPrec :: Int -> a -> PP.Doc
	pretty = prettyPrec 0
	prettyPrec _ = pretty
	
emptyPrinter ::  () ->  PP.Doc
emptyPrinter _ = PP.empty

maybePP :: (a -> PP.Doc) -> Maybe a -> PP.Doc
maybePP _ Nothing = PP.empty
maybePP pp (Just a) = pp a

instance Pretty Text where
    pretty str = PP.text $ T.unpack str

-- ------------------------------------------------------------
-- * Read and write
-- ------------------------------------------------------------

writeFields :: FilePath -> alpha -> [FieldDescriptionS alpha] -> IO ()
writeFields fpath date dateDesc = writeFile fpath (T.unpack $ showFields date dateDesc)

showFields ::  alpha  -> [FieldDescriptionS alpha] ->  Text
showFields date dateDesc = T.pack . PP.render $
    foldl' (\ doc (FDS _ printer _) ->  doc PP.$+$ printer date) PP.empty dateDesc

readFields :: FilePath -> [FieldDescriptionS alpha] -> alpha -> IO alpha
readFields fn fieldDescrs defaultValue = E.catch (do
    res <- P.parseFromFile (parseFields defaultValue fieldDescrs) fn
    case res of
                Left pe -> error $ "Error reading file " ++ show fn ++ " " ++ show pe
                Right r -> return r)
    (\ (e::IOException) -> error $ "Error reading file " ++ show fn ++ " " ++ show e)

parseFields ::  alpha ->  [FieldDescriptionS alpha] ->  P.CharParser () alpha
parseFields defaultValue descriptions =
    let parsersF = map fieldParser descriptions in
        applyFieldParsers defaultValue parsersF
        P.<?> "prefs parser"


