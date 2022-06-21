{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Example module docs
Description : Short description
Copyright   : (c) Christopher Gómez, 2022
    Nestor Javier, 2022
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Utils.Checkers (
    checkGuess
) where

import qualified Data.Set as Set
import Data.List (zipWith, transpose, delete)

{-|
  String de calificación de una adivinación de Wordle, tiene el siguiente
  formato:

  - T donde la letra de la adivinación se encuentra en la posición 
    correspondiente en la respuesta.
  - V donde la letra de la adivinación se encuentra en la respuesta pero en
    una posición distinta.
  - _ donde la letra de la adivinación no se encuentra en la respuesta.
-}
type QualificationString = String

{-|
  La función 'checkGuess' chequea la validez y califica una adivinación
  de Wordle.

  Recibe la adivinación, la respuesta y el conjunto de palabras válidas y
  retorna:
  
  - Left mensaje si la adivinación es inválida.
  - Right calificación si la adivinación es válida.

  Ejemplos:

    >>> checkGuess "CARAS" "CARPA"
    Right "TTTV_"
    >>>checkGuess "TIENE" "PEROL"
    Right "--V--"
    >>> checkGuess "PALTA" "RESTO"
    Right "---T-"
    >>> checkGuess "PERRO" "PERRO"
    Right "TTTTT"
    >>> checkGuess "TIENE" "TENER"
    Right"T-VVV"
    >>> checkGuess "SIEMPRE" "NUNCA"
    Left "El tamaño de las cadenas no coincide"
-}
checkGuess :: String -> String -> Set.Set String 
    -> Either String QualificationString
checkGuess guess answer words
    | guess == answer               = Right $ replicate (length guess) 'T'
    | length guess /= length answer = Left "El tamaño de la adivinación no coincide"
    | guess `Set.notMember` words   = Left "La palabra es inválida"
    | otherwise =
        Right $ checkV guess resString remChars
        where
            (resString, remChars) = checkT guess answer

{-|
  La función `checkT` chequea los toros de una palabra, dada la adivinación y
  la respuesta esperada.

  Retorna una tupla con:

  - La string de calificación parcial con el formato descrito en `checkGuess`
  - Una string con las letras restantes por adivinar.
-}
checkT :: String -> String
    -> (QualificationString, String)
checkT guess answer =
    let temp = zipWith (\x y -> if x == y then "T " else "-" ++ [y]) guess answer
        [resString, remChars] = transpose temp
    in (resString, filter (/= ' ') remChars)

{-|
  La función `checkV` chequea las vacas de la palabra, dada la adivinación, una
  string de calificación, y una string con las letras restantes por adivinar.

  Retorna una string de calificación.
--}
checkV :: String -> String -> String
    -> QualificationString
checkV "" _ _ = ""
checkV (h1:r1) (h2:r2) remAns
    | h2 == 'T'        = "T" ++ checkV r1 r2 remAns
    | h1 `elem` remAns = "V" ++ checkV r1 r2 (delete h1 remAns)
    | otherwise        = "-" ++ checkV r1 r2 remAns

{-

-}
-- validateGuess :: String -> String -> Set.Set String -> Either String QualificationString