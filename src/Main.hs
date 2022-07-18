{-|
Descripción : Entry-point de implementación de Wordle en Haskell.
    Se ejecuta con un solo argumento [mentemaestra|descifrador] para
    escoger el modo de juego
Copyright   : (c) Christopher Gómez, 2022
Licencia    : GPL-3
-}

import System.Environment
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )
import Wordle.Decoder
import Wordle.Mastermind
import Wordle.Utils.IOHelpers ( loadWords, getRandomWord )

wordsFile = "./Palabras.txt"

main = do
    -- Configura el buffering de la salida estándar para que ocurra la
    -- salida de inmediato
    hSetBuffering stdout NoBuffering
    let modes = ["mentemaestra", "descifrador"]

    -- Valida los argumentos de línea de comando
    if length args /= 1 || head args `notElem` modes
        then putStrLn "Uso: wordle <mentemaestra|descifrador>"
    else do
        -- Carga las palabras y selecciona una al azar
        words <- loadWords wordsFile
        randomWord <- getRandomWord words

        -- Selecciona el modo de juego
        args <- getArgs
        if head args == "mentemaestra"
            then do
                playMastermindMode words randomWord 6 []
            else do
                playDecoderMode randomWord (initWordSet words) initScoreSet 6