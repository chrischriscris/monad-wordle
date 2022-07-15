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

-- ============== DEFINICIONES DE TIPOS DE DATOS ==============

-- | Tipo de dato para representar una palabra.
data Guess = Guess String Int
    deriving (Show, Eq)

instance Ord Guess where
    compare (Guess w1 s1) (Guess w2 s2) = compare (s1, w1) (s2, w2)

-- | Tipo de dato para representar una string de calificación del usuario.
data Score = Score String Int
    deriving (Show, Eq)

instance Ord Score where
    compare (Score w1 s1) (Score w2 s2) = compare (s2, w2) (s1, w1)

-- =============== DEFINICIONES DE CONJUNTOS ==========================

-- | Crea el conjunto inicial con todas las Guess del juego, dadas estas
-- en un conjunto de String.
minimaxWords :: Set.Set String -> Set.Set Guess
minimaxWords = Set.map guessFromString

-- | Conjunto con todas las combinaciones de Scores posibles.
scoreCombinations :: Set.Set Score
scoreCombinations = Set.fromList [ scoreFromString str | str <- scoreStrings ]
    where
        x = "-VT"
        scoreStrings = [[a, b, c, d, e] | a<-x, b<-x, c<-x, d<-x, e<-x]


-- data Minimaxer = Minimaxer {
--     alphabet :: Set.Set Char,
--     wordlist :: [String],
--     maxDepth :: Int,
--     maxScore :: Int,
--     minScore :: Int,
--     maxGuess :: Guess,
--     minGuess :: Guess
-- }

-- qualifiedSet :: Set.Set String -> Set.Set (Int, String)
-- qualifiedSet = Set.map (\word -> (qualifyWord word, word))

-- =========== FUNCIONES PARA MANEJAR TIPOS DE DATOS ===========

-- | Retorna un Guess dada una palabra del conjunto de palabras, puntuada
-- de acuerdo al algoritmo de Minimax.
--
-- Usa enteros para salvar la eficiencia que puede perderse al manipular
-- números de punto flotante.
guessFromString :: String -> Guess
guessFromString word = Guess word (sum $ map qualifyChar word)
    where qualifyChar c
            | c `elem` "AE"    =  1
            | c `elem` "OSRIN" =  2
            | c `elem` "LUCTD" =  3
            | c `elem` "MPBG"  =  5
            | c `elem` "FHVYQ" =  8
            | c `elem` "JZXKW" = 10
            | otherwise        =  0

-- | Retorna un Score dado una string con la calificación del usuario,
-- puntuada de acuerdo al algoritmo de Minimax.
--
-- Usa enteros para salvar la eficiencia que puede perderse al manipular
-- números de punto flotante.
scoreFromString :: String -> Score
scoreFromString string = Score string (sum $ map qualifyChar string)
    where qualifyChar 'T'  = 0
          qualifyChar 'V'  = 1
          qualifyChar '-'  = 2
          qualifyChar  c   = error "Caracter inválido" -- Nunca debería ocurrir

-- | Recibe una String de calificación y retorna una lista de filtros
-- que servirá para filtrar del conjunto las palabras inválidas
interpretQualification :: String -> [String -> Bool]
interpretQualification str = let f x = True in [f]

-- | Dada una lista de funciones que reciben String y retornan booleano, y
-- una string, aplica todas las funciones de la lista a la string y retorna
-- un único booleano (fold fashion)
pruneFunction :: [String -> Bool] -> String -> Bool
pruneFunction [] _ = True
pruneFunction (f:fs) str = f str && pruneFunction fs str

-- | Cuenta el número de ocurrencias de un caracter en una String
count :: Char -> String -> Int
count c =  length . filter (==c)