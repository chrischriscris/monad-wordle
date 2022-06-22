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
import Data.Char -- necesaria apra noAccent

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
    | guess == answer               = Right "TTTTT"
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
-}
checkV :: String -> String -> String
    -> QualificationString
checkV "" _ _ = ""
checkV (h1:r1) (h2:r2) remAns
    | h2 == 'T'        = "T" ++ checkV r1 r2 remAns
    | h1 `elem` remAns = "V" ++ checkV r1 r2 (delete h1 remAns)
    | otherwise        = "-" ++ checkV r1 r2 remAns


{-|
    La funcion `validateGuess` verifica si una palabra es valida 
    Entrada: palabra a verificar, conjunto con las validas palabras
    Si la palabra no tiene 5 letras retorna false
    Si la palabra tiene algun caracter especial, retorna False
    Si la palabra no pertenece al conjunto, retorna False.
-}
validateGuess :: String -> Set.Set String -> Either String String
validateGuess guess words
    | length guess /= 5           = Left "La palabra no tiene 5 letras"
    | isSpecialChar guess         = Left "La palabra tiene caracteres especiales"
    | guess `Set.notMember` words = Left "La palabra no es válida"
    | otherwise                   = Right $ removeAccents (map toUpper guess)

{-|
    Funcion que verifica si una palabra posee caracteres especiales
-}
isSpecialChar :: String -> Bool
isSpecialChar = let f = (\x -> (||) (x < '\64' || x > '\90'))
    in foldr f False

{-|
  La función `removeAccents` remueve los acentos de una String.

  Ejemplo:

  >>> noAccent "Hola, cómo estás."
  "Hola, como estas."
-}
removeAccents :: String -> String
removeAccents = map removeAccentChar

removeAccentChar :: Char -> Char
removeAccentChar n |  --------------------------------------------- A
    n == '\xc0'   || n == '\xc1'   || n == '\xc2'   ||
    n == '\xc3'   || n == '\xc4'   || n == '\xc5'   ||
    n == '\x0100' || n == '\x0102' || n == '\x0104'    = 'A'
               |  ----------------------------------------------- a
    n == '\xe0'   || n == '\xe1'   || n == '\xe2'   ||
    n == '\xe3'   || n == '\xe4'   || n == '\xe5'   ||
    n == '\x0101' || n == '\x0103' || n == '\x0105'    = 'a'
               |  ----------------------------------------------- E
    n == '\xc8'   || n == '\xc9'   || n == '\xca'   ||
    n == '\xcb'   || n == '\x0112' || n == '\x0114' ||
    n == '\x0116' || n == '\x0118' || n == '\x011a'    = 'E'
               |  ----------------------------------------------- e
    n == '\xe8'   || n == '\xe9'   || n == '\xea'   ||
    n == '\xeb'   || n == '\x0113' || n == '\x0115' ||
    n == '\x0117' || n == '\x0119' || n == '\x011b'    = 'e'
               |  ----------------------------------------------- I
    n == '\xcc'   || n == '\xcd'   || n == '\xce'   ||
    n == '\xcf'   || n == '\x0128' || n == '\x012a' ||
    n == '\x012c' || n == '\x012e' || n == '\x0130'    = 'I'
               |  ----------------------------------------------- i
    n == '\xec'   || n == '\xed'   || n == '\xee'   ||
    n == '\xef'   || n == '\x0129' || n == '\x012b' ||
    n == '\x012d' || n == '\x012f' || n == '\x0131'    = 'i'
               |  ----------------------------------------------- O
    n == '\xd2'   || n == '\xd3'   || n == '\xd4'   ||
    n == '\xd5'   || n == '\xd6'   || n == '\xd8'   ||
    n == '\x014c' || n == '\x014e' || n == '\x0150'    = 'O'
               |  ----------------------------------------------- o
    n == '\xf2'   || n == '\xf3'   || n == '\xf4'   ||
    n == '\xf5'   || n == '\xf6'   || n == '\xf8'   ||
    n == '\x014d' || n == '\x014f' || n == '\x0151'    = 'o'
               |  ----------------------------------------------- U
    n == '\xd9'   || n == '\xda'   || n == '\xdb'   ||
    n == '\xdc'   || n == '\x0168' || n == '\x016a' ||
    n == '\x016c' || n == '\x016e' || n == '\x0170' ||
    n == '\x0172'                                      = 'U'
               |  ----------------------------------------------- u
    n == '\xf9'   || n == '\xfa'   || n == '\xfb'   ||
    n == '\xfc'   || n == '\x0169' || n == '\x016b' ||
    n == '\x016d' || n == '\x016f' || n == '\x0171' ||
    n == '\x0173'                                      = 'u'
               | otherwise = n