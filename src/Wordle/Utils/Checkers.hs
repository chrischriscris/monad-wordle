{-|
Módulo      : Wordle.Utils.Checker
Descripción : Chequeos para implementación del modo Mentemaestra de Wordle
    en Haskell.
Copyright   : (c) Christopher Gómez, 2022
Licencia    : GPL-3
-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Wordle.Utils.Checkers (
    checkGuess
) where

import Data.Set ( Set, notMember )
import Data.List ( zipWith, transpose, delete )
import Data.Char ( isAlpha, toUpper )
import Data.Either ( isLeft )


-- ========== ALIAS DE TIPOS PARA DOCUMENTACIÓN ==========

-- | String de calificación de una adivinación de Wordle, tiene el siguiente
-- formato:
--
-- * T donde la letra de la adivinación se encuentra en la posición 
--   correspondiente en la respuesta.
-- * V donde la letra de la adivinación se encuentra en la respuesta pero en
--   una posición distinta.
-- * - donde la letra de la adivinación no se encuentra en la respuesta.
type ScoreString = String

type Guess = String
type Answer = String

-- ========== INTERFAZ EXTERNA DEL MÓDULO ==========

-- | Chequea la validez y califica una adivinación de Wordle.
--
-- Recibe la adivinación, la respuesta y el Set de palabras válidas y
-- retorna:
--
-- * Left mensaje si la adivinación es inválida.
-- * Right calificación si la adivinación es válida.
--
-- Ejemplos:
--
-- >>> checkGuess "CARAS" "CARPA"
-- Right "TTTV_"
-- >>>checkGuess "TIENE" "PEROL"
-- Right "--V--"
-- >>> checkGuess "PALTA" "RESTO"
-- Right "---T-"
-- >>> checkGuess "PERRO" "PERRO"
-- Right "TTTTT"
-- >>> checkGuess "TIENE" "TENER"
-- Right"T-VVV"
-- >>> checkGuess "SIEMPRE" "NUNCA"
-- Left "El tamaño de las cadenas no coincide"
checkGuess :: Guess -> Answer -> Set String
    -> Either String ScoreString
checkGuess guess answer words
    | isLeft res       = res -- Propaga el error
    | otherwise        =
        let Right word = res in
            if word == answer then Right "TTTTT"
            else Right $ checkV word $ checkT word answer
    where
        res = validateGuess guess words


-- ========== FUNCIONES DE CHEQUEO ==========

-- | Chequea los toros de una palabra, dada la adivinación y la respuesta
-- esperada.
--
-- Retorna una tupla con:
--
-- * Una string con las letras restantes por adivinar.
-- * La string de calificación parcial con el formato descrito en `checkGuess`
checkT :: Guess -> Answer
    -> (ScoreString, String)
checkT guess answer =
    let temp = zipWith (\x y -> if x == y then "T" else "-" ++ [y]) guess answer
        [resString, remChars] = transpose temp
    in (resString, remChars)

-- | Chequea las vacas de la palabra, dado el resultado de haber
-- chequeado los toros con la función `checkT` previamente.
-- 
-- Retorna una string de calificación.
checkV :: Guess -> (ScoreString, String)
    -> ScoreString
checkV "" _ = []
checkV (h1:r1) (h2:r2, remAns)
    | h1 `elem` remAns = 'V' : checkV r1 (r2, delete h1 remAns)
    | otherwise        = (if h2 == 'T' then 'T' else '-') : checkV r1 (r2, remAns)

-- ========== FUNCIONES DE VALIDACIÓN ==========

-- | Verifica si una palabra es valida, dadas la palabra y un Set con
-- las palabras válidas.
-- 
-- Retorna:
--
-- * Left mensaje si la palabra no es válida, con el mensaje de error.
-- * Right palabra si la palabra es válida, con la palabra en mayúsculas
-- y sin acentos.
-- 
-- Una palabra es válida si:
--
-- * Tiene 5 caracteres.
-- * Todos sus caracteres son alfabéticos
-- * Pertenece al conjunto de palabras válidas.
validateGuess :: Guess -> Set String -> Either String Guess
validateGuess guess words
    | length guess /= 5         = Left "La palabra no tiene 5 letras"
    | not $ isAlphabetic guess  = Left "La palabra tiene caracteres especiales"
    | word `notMember` words    = Left "La palabra no es válida"
    | otherwise                 = Right word
    where
        word = uniformize guess

-- | Verifica si una String es alfabética.
isAlphabetic :: String -> Bool
isAlphabetic = all isAlpha

-- | Convierte una String a mayúsculas y remueve los acentos agudos de
-- sus vocales.
-- 
-- Ejemplo:
-- 
-- >>> uniformize "Hola, cómo estás."
-- "HOLA, COMO ESTAS."
uniformize :: String -> String
uniformize = map (removeAccent . toUpper)
    where
        removeAccent 'Á' = 'A'
        removeAccent 'É' = 'E'
        removeAccent 'Í' = 'I'
        removeAccent 'Ó' = 'O'
        removeAccent 'Ú' = 'U'
        removeAccent  c  =  c