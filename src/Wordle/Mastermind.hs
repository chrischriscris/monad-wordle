{-|
Módulo      : Wordle.Decoder
Descripción : Implementación del modo mentemaestra de Wordle.
    en Haskell.
Copyright   : (c) Christopher Gómez, 2022
    Nestor Javier, 2022
Licencia    : GPL-3
-}

module Wordle.Mastermind where

import Data.Set ( Set )
import Data.Either ( isLeft )
import Wordle.Utils.Checkers
import Wordle.Utils.IOHelpers ( printHistory )

-- | Ejecuta el modo mentemaestra del juego.
--
-- Argumentos:
--
-- * El conjunto de palabras del juego.
-- * La palabra a adivinar.
-- * El número de intentos restantes.
-- * Lista de Strings con las calificaciones parciales.
playMastermindMode :: Set String -> String -> Int -> [String] -> IO ()
playMastermindMode words answer lives history = do
    if lives /= 0 then do
        putStrLn $ "Intentos restantes: " ++ show lives
        -- Obtiene la palabra del jugador y evalúa su respuesta
        putStr "DESCIFRADOR : "
        guess <- getLine
        let eval = checkGuess guess answer words

        if isLeft eval then do
            -- Si es error se indica y continúa con los mismo intentos
            let Left error = eval
            putStrLn $ "Error: " ++ error ++ "\n"
            playMastermindMode words answer lives history

        else do
            -- Si es válida, imprime la string de calificación
            let Right score = eval
            putStr "MENTEMAESTRA: "
            putStrLn score

            if score == "TTTTT" then do
                -- Si el jugador ha acertado, imprime la palabra y termina
                putStrLn "\n¡Ganaste!\n"
                printHistory (score:history)
            else do
                putStr "\n"
                playMastermindMode words answer (lives - 1) (score:history)
    else do
        -- Si el jugador no tiene más intentos, revela la palabra
        putStrLn $ "La palabra era \"" ++ answer ++ "\""