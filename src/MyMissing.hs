{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  MyMissing
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info@leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for missing base functions
--
------------------------------------------------------------------------------

module MyMissing (
    allOf
,   forceJust
,   forceHead
,   split
,   replace
,   nonEmptyLines
,   trim
) where

import Data.List (find,unfoldr)
import Data.Maybe (isJust)
import Data.Char (isSpace)


-- | remove leading and trailing spaces
trim :: String -> String
trim      = f . f
   where f = reverse . dropWhile isSpace

nonEmptyLines :: String -> [String]
nonEmptyLines = filter (\line -> isJust $ find (not . isSpace) line) . lines


allOf :: forall alpha. (Bounded alpha, Enum alpha) =>  [alpha]
allOf = map toEnum [fromEnum (minBound :: alpha) .. fromEnum (maxBound :: alpha)]

-- ---------------------------------------------------------------------
-- Convenience methods with error handling
--
forceJust :: Maybe alpha -> String -> alpha
forceJust mb str = case mb of
			Nothing -> error str
			Just it -> it

-- ---------------------------------------------------------------------
-- Convenience methods with error handling
--
forceHead :: [alpha] -> String -> alpha
forceHead (h:_) str = h
forceHead [] str = error str


-- ---------------------------------------------------------------------
-- Splitting a string into parts based on a token delimiter
--

split :: Eq a => a -> [a] -> [[a]]
split =  unfoldr . split'

split' :: Eq a => a -> [a] -> Maybe ([a], [a])
split' c l
  | null l = Nothing
  | otherwise = Just (h, drop 1 t)
  where (h, t) = span (/=c) l

-- ---------------------------------------------------------------------
-- Simple replacement
--

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace from to xs@(a:as) =
    if isPrefixOf from xs
        then to ++ replace from to (drop (length from) xs)
        else a : replace from to as
    where isPrefixOf as bs = and $ zipWith (== ) as bs
