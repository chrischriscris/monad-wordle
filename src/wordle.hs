import System.Environment
import System.IO
import Data.Set ( Set )
import Data.Either ( isLeft )
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
        then putStrLn "Uso: wordle <mentemaestra|descifrador>"
    else do
        randomWord <- getRandomWord words
        if head args == "mentemaestra"
            then do
                mastermindMode words randomWord 6 []
            else do
                decoderMode randomWord (initWordSet words) initScoreSet 6

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
--
-- Argumentos:
--
-- * El intento de adivinación actual.
-- * El conjunto de posibles adivinaciones en el contexto.
-- * El conjunto de posibles strings de Score del usuario en el contexto.
-- * El número de intentos restantes.
decoderMode :: String -> Set Guess -> Set Score -> Int -> IO ()
decoderMode guess wordSet scoreSet lives = do
    if lives /= 0 then do
        putStrLn $ "Intento " ++ show (7 - lives) ++ ":"

        -- Presenta al usuario la adivinación
        putStrLn $ "DESCIFRADOR  : " ++ guess
        putStr "MENTEMAESTRA : "
        userScore <- getLine

        if userScore == "TTTTT" then do
            -- Si la computadora ha acertado, termina
            putStrLn "\n¡Gané! :)\n"
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
                    decoderMode guess wordSet scoreSet lives
            else do
                -- Si es válida, pasa al siguiente intento
                let Right (nextGuess, wordSet', scoreSet') = eval

                putStr "\n"
                decoderMode nextGuess wordSet' scoreSet' (lives - 1)
    else do
        -- Si el jugador no tiene más intentos, revela la palabra
        putStrLn "\n¡Ganaste! :("