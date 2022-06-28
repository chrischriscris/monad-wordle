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

import Data.Set ( Set, notMember )
import Data.List ( zipWith, transpose, delete )
import Data.Char ( isAlpha, toUpper )
import Data.Either ( isLeft, fromRight )

qualifyWord :: String -> Int
qualifyWord word = foldl' (+)

qualifyChar :: Char -> Int
qualifyChar c
    | c `elem` "AE"    = 1
    | c `elem` "OSRIN" = 2
    | c `elem` "LUCTD" = 3
    | c `elem` "MPBG"  = 5
    | c `elem` "FHVYQ" = 8
    | c `elem` "JZXKW" = 10
    | otherwise        = 0