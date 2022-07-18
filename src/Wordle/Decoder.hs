{-|
Módulo      : Wordle.Decoder
Descripción : Implementación del modo descifrador de Wordle.
    en Haskell.
Copyright   : (c) Christopher Gómez, 2022
Licencia    : GPL-3
-}

module Wordle.Decoder where

import Data.Set ( Set, fromList )
import qualified Data.Set as Set
import Data.Either ( isLeft )
import Wordle.Utils.Minimaxer

-- | Ejecuta el modo descifrador del juego.
--
-- Argumentos:
--
-- * El intento de adivinación actual.
-- * El conjunto de posibles adivinaciones en el contexto.
-- * El conjunto de posibles strings de Score del usuario en el contexto.
-- * El número de intentos restantes.
playDecoderMode :: String -> Set Guess -> Set Score -> Int -> IO ()
playDecoderMode guess wordSet scoreSet lives = do
    if lives /= 0 then do
        putStrLn $ "Intento " ++ show (7 - lives) ++ ":"

        -- Presenta al usuario la adivinación
        putStrLn $ "DESCIFRADOR  : " ++ guess
        putStr "MENTEMAESTRA : "
        userScore <- getLine

        if userScore == "TTTTT" then do
            -- Si la computadora ha acertado, termina
            putStrLn "\n¡Gané! :)"
        else do
            let eval = guessNext guess userScore wordSet scoreSet

            if isLeft eval then do
                let Left error = eval
                if error == "T" then do
                    -- Si el usuario hacía trampa se indica y sale del juego
                    putStrLn "\n¡Tramposo! ›:("
                else do
                    -- Si es error se indica y continúa con los mismo intentos
                    putStrLn $ "Error: " ++ error ++ "\n"
                    playDecoderMode guess wordSet scoreSet lives
            else do
                -- Si es válida, pasa al siguiente intento
                let Right (nextGuess, wordSet', scoreSet') = eval

                putStr "\n"
                playDecoderMode nextGuess wordSet' scoreSet' (lives - 1)
    else do
        -- Si el jugador no tiene más intentos, revela la palabra
        putStrLn "\n¡Ganaste! :("

-- =============== DEFINICIONES DE CONJUNTOS ==========================

-- | Crea el conjunto inicial con todas las Guess del juego, dadas estas
-- en un conjunto de String.
initWordSet :: Set String -> Set Guess
initWordSet = Set.map guessFromString

-- | Conjunto con todas las combinaciones de Scores posibles.
initScoreSet :: Set Score
initScoreSet = fromList [ scoreFromString str | str <- scoreStrings ]
    where
        x = "-VT"
        scoreStrings = [[a, b, c, d, e] | a<-x, b<-x, c<-x, d<-x, e<-x]