import System.Environment ( getArgs )
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )
import Wordle.Decoder
import Wordle.Mastermind
import Wordle.Utils.IOHelpers ( loadWords, getRandomWord )

wordsFile = "./Palabras.txt"

-- | Entry-point de implementación de Wordle en Haskell.
--
-- Lee un solo argumento de línea de comandos, 'mentemaestra' o 'descifrador'
-- para escoger el modo de juego
main = do
    -- Configura el buffering de la salida estándar para que ocurra la
    -- salida de inmediato
    hSetBuffering stdout NoBuffering
    let modes = ["mentemaestra", "descifrador"]
    args <- getArgs

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