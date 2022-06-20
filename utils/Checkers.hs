{-|
Module      : Example module docs
Description : Short description
Copyright   : (c) Christopher Gómez, 2022
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Checker (
    checkGuess
) where

import Data.List

{-|
  La función 'checkGuess' recibe un String a comprobar y la respuesta correcta,
  de igual tamaño, y retorna una String con el resultado de la comprobación,
  con el siguiente formato:

  - T donde la letra de la adivinación se encuentra en la posición 
    correspondiente en la respuesta.
  - V donde la letra de la adivinación se encuentra en la respuesta pero en
    una posición distinta.
  - _ donde la letra de la adivinación no se encuentra en la respuesta.

  Por ejemplo:
    checkGuess "CARAS" "CARPA" = "TTTV_"
    checkGuess "TIENE" "PEROL" = "--V--"
    checkGuess "PALTA" "RESTO" = "---T-"
    checkGuess "PERRO" "PERRO" = "TTTTT"
    checkGuess "TIENE" "TENER" -> "T-VVV"
    checkGuess "SIEMPRE" "NUNCA" -> error "El tamaño de las palabras no coincide"
-}
checkGuess :: String -> String -> String
checkGuess guess answer
    | guess == answer = take (length guess) (repeat 'T')
    | length guess /= length answer = error "El tamaño de las cadenas no coincide"
    | otherwise =
        checkV guess resultString remainingChars
        where
            (resultString, remainingChars) = checkT guess answer answer

-- Chequea los toros de la palabra
-- checkT guess asnwer remainingChars
checkT :: String -> String -> String -> (String, String)
checkT "" "" remAns = ("", remAns)

checkT (h1:r1) (h2:r2) ans
    | h1 == h2  = ("T" ++ remGuess, delete h1 remAns)
    | otherwise = ("-" ++ remGuess, remAns)
    where
        (remGuess, remAns) = checkT r1 r2 ans

-- Chequea las vacas de la palabra
-- checkV guess resultString remainingChars
checkV :: String -> String -> String -> String
checkV "" _ _ = ""
checkV (h1:r1) (h2:r2) remAns
    | h2 == 'T'        = "T" ++ checkV r1 r2 remAns
    | h1 `elem` remAns = "V" ++ checkV r1 r2 (delete h1 remAns)
    | otherwise        = "-" ++ checkV r1 r2 remAns