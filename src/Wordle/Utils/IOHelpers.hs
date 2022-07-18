{-|
Módulo      : Utils.IOHelpers
Descripción : Funciones de ayuda de IO para implementación de Wordle.
    en Haskell.
Copyright   : (c) Christopher Gómez, 2022
    Nestor Javier, 2022
Licencia    : GPL-3
-}

module Wordle.Utils.IOHelpers where

import System.IO
import System.Random
import Data.Set ( Set, elemAt, fromDistinctAscList )

-- | Carga el archivo de palabras en un Set.
loadWords :: FilePath -> IO (Set String)
loadWords path = do
    contents <- readFile path
    let words = fromDistinctAscList $ lines contents
    return words

-- | Retorna una String aleatoria de un Set de Strings.
getRandomWord :: Set String -> IO String
getRandomWord words = do
    let n = length words
    -- Obtiene un generador pseudo-aleatorio del sistema operativo
    gen <- getStdGen

    -- Obtiene y retorna una palabra aleatoria
    return $ elemAt (fst $ randomR (0, n-1) gen) words

-- | Imprime por salida estándar el historial de una partida de Wordle.
printHistory :: [String] -> IO ()
printHistory history = do
    putStrLn "Comparte tu resultado:"
    mapM_ putStrLn $ reverse history