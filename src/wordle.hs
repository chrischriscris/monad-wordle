import System.Environment
import Control.Monad
import System.IO
import System.Random
import System.Exit
import Data.List
import Utils.Checkers
import qualified Data.Set as Set

wordsFile = "./Palabras.txt"

main = do
    -- Configura el buffering de la salida estándar para que ocurra la
    -- salida de inmediato
    hSetBuffering stdout NoBuffering
    let modes = ["mentemaestra", "descifrador"]

    words <- loadWords wordsFile
    args <- getArgs
    if (length args /= 1) || (not $ (args !! 0) `elem` modes)
        then die "Uso: wordle [mentemaestra|descifrador]"
    else do
        if (args !! 0) == "mentemaestra"
            then do
                randomWord <- getRandomWord words
                mastermindMode words randomWord 6
        else do
            -- decoderMode
            putStrLn "TODO"

{-|
  La función `loadWords` carga el archivo de palabras y devuelve una tupla con
  la lista de palabras y el número de palabras obtenidas.
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
    if lives /= 0
        then do 
            putStr "DESCIFRADOR : "
            guess <- getLine

            putStr "MENTEMAESTRA: "
            let score = checkGuess guess answer
            putStrLn score

            if score == replicate (length guess) 'T'
                then do
                    putStrLn "\n¡Ganaste!"
            else do
                putStr "\n"
                mastermindMode words answer (lives - 1)
    else do
        putStrLn $ "La palabra era \"" ++ answer ++ "\""
        

{-|
  La función `getRandomWord` devuelve una String aleatoria de un Set de Strings
-}
getRandomWord :: Set.Set String -> IO (String)
getRandomWord words = do
    let n = length words
    -- Obtiene un generador pseudo-aleatorio del sistema operativo
    gen <- getStdGen
    -- Obtiene y retorna una palabra aleatoria
    return $ Set.elemAt (fst $ randomR (0, n-1) gen) words