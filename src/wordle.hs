import System.Environment
import Control.Monad
import System.IO
import System.Random
import System.Exit
import Data.List
import Utils.Checkers
import qualified Data.Set as Set
import Data.Either

wordsFile = "./Palabras.txt"

main = do
    -- Configura el buffering de la salida estándar para que ocurra la
    -- salida de inmediato
    hSetBuffering stdout NoBuffering
    let modes = ["mentemaestra", "descifrador"]

    words <- loadWords wordsFile
    args <- getArgs
    if length args /= 1 || head args `notElem` modes
        then die "Uso: wordle [mentemaestra|descifrador]"
    else do
        if head args == "mentemaestra"
            then do
                randomWord <- getRandomWord words
                mastermindMode words randomWord 6
        else do
            -- decoderMode
            putStrLn "TODO"

{-|
  La función `loadWords` carga el archivo de palabras y devuelve una tupla con
  un Set que contiene la lista de palabras.
-}
loadWords :: FilePath -> IO (Set.Set String)
loadWords path = do
    contents <- readFile path
    let words = Set.fromDistinctAscList $ lines contents
    return words

{-|
  La función `mastermindMode` ejecuta el modo mentemaestra del juego.

  Recibe el conjunto de palabras del juego, el número de intentos que el
  jugador tendrás para adivinar la palabra al azar escogida.
-}
mastermindMode :: Set.Set String -> String -> Int -> IO ()
mastermindMode words answer lives = do
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
            mastermindMode words answer lives

        else do
            -- Si es válida, imprime la string de calificación
            let Right score = eval
            putStr "MENTEMAESTRA: "
            putStrLn score

            if score == replicate (length guess) 'T' then do
                -- Si el jugador ha acertado, imprime la palabra y termina
                putStrLn "\n¡Ganaste!"
            else do
                putStr "\n"
                mastermindMode words answer (lives - 1)
    else do
        -- Si el jugador no tiene más intentos, revela la palabra
        putStrLn $ "La palabra era \"" ++ answer ++ "\""


{-|
  La función `getRandomWord` devuelve una String aleatoria de un Set de Strings
-}
getRandomWord :: Set.Set String -> IO String
getRandomWord words = do
    let n = length words
    -- Obtiene un generador pseudo-aleatorio del sistema operativo
    gen <- getStdGen
    -- Obtiene y retorna una palabra aleatoria
    return $ Set.elemAt (fst $ randomR (0, n-1) gen) words