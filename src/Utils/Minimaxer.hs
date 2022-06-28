{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Módulo      : Utils.Minimaxer
Descripción : Motor de minimax para implementación de Wordle.
    en Haskell.
Copyright   : (c) Christopher Gómez, 2022
    Nestor Javier, 2022
Licencia    : GPL-3
-}
module Utils.Minimaxer where

import qualified Data.Set as Set
import Data.List ( foldl' )
import Data.Char ( isAlpha, toUpper )
import Data.Either ( isLeft, fromRight )

qualifiedSet = Set.map (\word -> (qualifyWord word, word))

qualifyWord :: String -> Int
qualifyWord word = sum $ map qualifyChar word

qualifyChar :: Char -> Int
qualifyChar c
    | c `elem` "AE"    = 1
    | c `elem` "OSRIN" = 2
    | c `elem` "LUCTD" = 3
    | c `elem` "MPBG"  = 5
    | c `elem` "FHVYQ" = 8
    | c `elem` "JZXKW" = 10
    | otherwise        = 0