import System.Environment
import Control.Monad
import System.IO
import System.Exit
import Data.List
import Data.Set ( Set, elemAt, fromDistinctAscList )
import Data.Either
import Utils.Checkers
import Utils.Minimaxer
import Utils.IOHelpers

wordsFile = "./Palabras.txt"

main = do
    -- Configura el buffering de la salida estándar para que ocurra la
    -- salida de inmediato
    hSetBuffering stdout NoBuffering
    let modes = ["mentemaestra", "descifrador"]

    words <- loadWords wordsFile
    args <- getArgs
    if length args /= 1 || head args `notElem` modes
        then die "Uso: wordle <mentemaestra|descifrador>"
    else do
        if head args == "mentemaestra"
            then do
                randomWord <- getRandomWord words
                mastermindMode words randomWord 6 []
        else do
            -- decoderMode
            putStrLn "TODO"

-- | Ejecuta el modo mentemaestra del juego.
--
-- Argumentos:
--
-- * El conjunto de palabras del juego.
-- * La palabra a adivinar.
-- * El número de intentos restantes.
-- * Lista de Strings con las calificaciones parciales.
mastermindMode :: Set String -> String -> Int -> [String] -> IO ()
mastermindMode words answer lives history = do
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
            mastermindMode words answer lives history

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
                mastermindMode words answer (lives - 1) (score:history)
    else do
        -- Si el jugador no tiene más intentos, revela la palabra
        putStrLn $ "La palabra era \"" ++ answer ++ "\""

-- | Ejecuta el modo descifrador del juego.
-- decoderMode :: a