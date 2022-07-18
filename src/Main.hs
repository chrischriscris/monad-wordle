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

    words <- loadWords wordsFile
    args <- getArgs
    if length args /= 1 || head args `notElem` modes
        then putStrLn "Uso: wordle <mentemaestra|descifrador>"
    else do
        randomWord <- getRandomWord words
        if head args == "mentemaestra"
            then do
                playMastermindMode words randomWord 6 []
            else do
                playDecoderMode randomWord (initWordSet words) initScoreSet 6