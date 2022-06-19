import System.Environment
import Control.Monad
import System.IO
import Data.List

wordsFile = "./Palabras.txt"

main = do
    (words, n) <- loadWords wordsFile
    mapM_ print words
    putStrLn $ "Número de palabras: " ++ show n

-- Carga el archivo de palabras y devuelve una lista de palabras y el número de
-- palabras
loadWords :: FilePath -> IO ([String], Int)
loadWords path = do
    contents <- readFile path
    let words = lines contents
    return (words, length words)