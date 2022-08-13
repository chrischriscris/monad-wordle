{-|
Módulo      : Wordle.Utils.Minimaxer
Descripción : Motor de minimax para implementación de Wordle.
    en Haskell.
Copyright   : (c) Christopher Gómez, 2022
Licencia    : GPL-3
-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module Wordle.Utils.Minimaxer ( 
    Guess, Hint,
    scoreFromString, guessFromString,
    guessNext
) where

import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Map ( toList, fromListWith )

-- $doc
-- == Definiciones de tipos de datos

-- | Tipo de dato para representar una palabra.
data Guess = Guess String Int
    deriving (Show, Eq)

instance Ord Guess where
    compare (Guess w1 s1) (Guess w2 s2) = compare (s1, w1) (s2, w2)

-- | Obtiene la String de un Guess.
guessString :: Guess -> String
guessString (Guess word _) = word

-- | Tipo de dato para representar una string de calificación del usuario.
data Hint = Hint String Int
    deriving (Show, Eq)

instance Ord Hint where
    compare (Hint w1 s1) (Hint w2 s2) = compare (s2, w2) (s1, w1)

-- | Obtiene la String de un Hint.
scoreString :: Hint -> String
scoreString (Hint string _) = string

-- $doc
-- == Funciones para manejar tipos de datos

-- | Retorna un Guess dada una palabra del conjunto de palabras, puntuada
-- de acuerdo al algoritmo de Minimax.
--
-- Usa enteros para salvar la eficiencia que puede perderse al manipular
-- números de punto flotante.
guessFromString :: String -> Guess
guessFromString word = Guess word (sum $ map qualifyChar word)
    where qualifyChar c
            | c `elem` "AE"    =  1
            | c `elem` "OSRIN" =  2
            | c `elem` "LUCTD" =  3
            | c `elem` "MPBG"  =  5
            | c `elem` "FHVYQ" =  8
            | c `elem` "JZXKW" = 10

-- | Retorna un Hint dado una string con la calificación del usuario,
-- puntuada de acuerdo al algoritmo de Minimax.
--
-- Usa enteros para salvar la eficiencia que puede perderse al manipular
-- números de punto flotante.
scoreFromString :: String -> Hint
scoreFromString string = Hint string (sum $ map qualifyChar string)
    where qualifyChar 'T'  = 0
          qualifyChar 'V'  = 1
          qualifyChar '-'  = 2

-- $doc
-- == Funciones misceláneas

-- | Verifica si una string de calificación del usuario es válida.
-- 
-- Retorna:
--
-- * Just mensaje si la string no es válida, con el mensaje de error.
-- * Nothing string si la string es válida.
-- 
-- Una palabra es válida si:
--
-- * Tiene 5 caracteres.
-- * Solo contiene los caracteres '-', 'V' y 'T'.
validateScore :: String -> Maybe String
validateScore score
    | length score /= 5
        = Just "La calificación debe contener 5 caracteres."
    | any (`notElem` "TV-") score
        = Just "La calificación solo puede contener los caracteres -, V y T."
    | otherwise = Nothing

-- | Cuenta el número de ocurrencias de un elemento en una lista.
count :: (Eq a) => a -> [a] -> Int
count c = length . filter (==c)

-- | Retorna un booleano indicando si un Hint es válido dada la string
-- de adivinación, el conjunto de adivinaciones posibles y el Hint.
isValidScore :: String ->  Set Guess -> Hint -> Bool
isValidScore guess guessSet score =
    -- Es válido si el conjunto resultante de filtrar las posibles
    -- adivinaciones usando ese Hint tiene al menos un elemento
    not $ Set.null (filterGuessSet guessSet guess $ scoreString score)

-- $doc
-- == Filtrado del conjunto de palabras

-- | Filtra el conjunto de palabras posibles según las Strings dadas.
--
-- Recibe el conjunto inicial, la palabra adivinada y la puntuación
-- del usuario.
filterGuessSet :: Set Guess -> String -> String -> Set Guess
filterGuessSet words guess score =
    let fils = getFilters guess score
        f = unifyFilters fils
    in Set.filter (\(Guess str _) -> f str) words


-- | Recibe un Guess y un Hint del usuario y retorna una lista de filtros
-- que servirá para filtrar del conjunto las palabras inválidas
getFilters :: String -> String -> [String -> Bool]
getFilters guess score = filters1 ++ filters2
    where
        filters1 = posFilters guess score 0
        filters2 = freqFilters guess score

-- | Función auxiliar que genera filtros posicionales.
posFilters :: String -> String -> Int -> [String -> Bool]
posFilters "" _ _ = []
posFilters (c1:w) (c2:sc) i
    | c2 == 'T' = (\str -> str !! i == c1) : posFilters w sc (i+1)
    | otherwise = (\str -> str !! i /= c1) : posFilters w sc (i+1)

-- | Función auxiliar que genera filtros de frecuencias.
freqFilters :: String -> String -> [String -> Bool]
freqFilters guess score = getFilters freqs
    where freqs = frequency (zip guess score)
          getFilters [] = []
          getFilters (x:xs) = case x of
            (c, 0) -> (\str -> count c str == 0) : getFilters xs
            (c, n) -> (\str -> count c str >= n)  : getFilters xs

-- | Función auxiliar de freqFilters.
frequency :: [(Char, Char)] -> [(Char, Int)]
frequency zipList = toList $
    fromListWith (+)
        [(c1, n) | (c1, c2) <- zipList,
                    let n = if c2 `elem` "VT" then 1 else 0]

-- $doc
-- == Filtrado del conjunto de puntuaciones posibles

-- | Filtra el conjunto de Scores posibles tras la Hint dada.
--
-- Recibe el conjunto inicial y la puntuación del usuario.
filterScoreSet :: Set Hint -> String -> Set Hint
filterScoreSet scores str =
    let f = unifyFilters $ freqFilter str : posFilters' str 0  in
        Set.filter (\(Hint sc _) -> f sc) scores

-- | Función auxiliar que genera filtros posicionales.
posFilters' :: String -> Int -> [String -> Bool]
posFilters' "" _ = []
posFilters' (c:cs) i
    | c == 'T' = (\str -> str !! i == c) : posFilters' cs (i+1)
    | otherwise = posFilters' cs (i+1)

-- | Función auxiliar que genera un filtro de frecuencia.
freqFilter :: String -> (String -> Bool)
freqFilter score = \str -> length (filter f str) >= n
    where
        f x = x `elem` "VT"
        n = length (filter f score)

-- | Dada una lista de funciones que reciben String y retornan booleano, y
-- una string, aplica todas las funciones de la lista a la string y retorna
-- un único booleano (fold fashion)
unifyFilters :: [String -> Bool] -> String -> Bool
unifyFilters [] _       = True
unifyFilters (f:fs) str = f str && unifyFilters fs str

-- $doc
-- == Agente Minimax

-- $doc
-- == Definición de árbol y sus nodos

-- | Tipo de dato para representar un árbol con cantidad no acotada
-- de nodos hijos.
data Rose a = Leaf a | Node a [Rose a]
    deriving Show

-- | Tipo de dato para que representa un nodo del agente Minimax.
data MinimaxNode = MinimaxNode {
    value    :: String,    -- ^ Valor del nodo (una palabra o una puntuación).
    wordSet  :: Set Guess, -- ^ Conjunto de Guess válidos al momento.
    scoreSet :: Set Hint, -- ^ Conjunto de Scores válidos al momento.
    score    :: Int        -- ^ Puntuación minimax del nodo.
} deriving (Show, Eq)

-- Nodos se ordenan según su puntuación
instance Ord MinimaxNode where
    compare a b = compare (score a) (score b)

-- | Obtiene el valor de un nodo de Rose.
getNodeVal :: Rose a -> a
getNodeVal (Leaf val  ) = val
getNodeVal (Node val _) = val

-- $doc
-- == Generación e interpretación del árbol Minimax

-- | Genera un nivel de minimización de árbol de Minimax, dado un nodo padre
-- y el nivel de profundidad.
-- 
-- Un nivel de minimización es una lista de nodos hijo, donde cada nodo
-- contiene las 10 palabras con menor puntuación que se pueden adivinar
-- dada la información del padre.
generateMinLevel :: MinimaxNode -> Int -> [Rose MinimaxNode]
generateMinLevel node level =
    -- Extrae la información del padre
    let (MinimaxNode _ wSet sSet sc) = node
        -- Toma las 10 palabras con menor puntuación
        words = Set.toList $ Set.take 10 wSet

        -- Genera con esas palabras lista de MinimaxNode
        nodeList = map
            (\(Guess w sc) -> MinimaxNode w wSet sSet sc) words

    -- Mapea cada nodo de la lista a un Rose rama
    in map (\n -> let nextLevel = generateMaxLevel n (level+1) in
        if null nextLevel then Leaf n else Node n nextLevel) nodeList

-- | Genera un nivel de maximización de árbol de Minimax, dado un nodo padre
-- y el nivel de profundidad.
-- 
-- Un nivel de maximización es una lista de nodos hijo, donde cada nodo
-- contiene las 10 strings de score con mayor puntuación que son posibles
-- respuestas del usuario, dada la información del padre.
generateMaxLevel :: MinimaxNode -> Int -> [Rose MinimaxNode]
generateMaxLevel node level =
    -- Extrae la información del padre
    let (MinimaxNode val wSet sSet _) = node
        -- Toma los 10 Scores válidos con menor puntuación
        scores = Set.take 10 $ Set.filter (isValidScore val wSet) sSet

        -- Genera con esos Scores lista de MinimaxNode
        nodeList = map
            (\(Hint str sc) -> MinimaxNode
                str
                (filterGuessSet wSet val str)
                (filterScoreSet sSet str)
                sc)
            (Set.toList scores)

    in
        -- Mapea cada nodo de la lista a un Rose hoja si ya hay 4 niveles de profundidad
        if level == 4 then map Leaf nodeList

        -- De otra forna, mapea cada nodo de la lista a un Rose rama
        else map (\n -> let nextLevel = generateMinLevel n (level+1) in
            if null nextLevel then Leaf n else Node n nextLevel) nodeList

-- | Interpreta un árbol minimax dado.
--
-- Incrementa recursivamente a cada nodo la puntuación de sus nodos hijos.
-- Escoje el nodo del primer nivel con menor puntuación.
-- 
-- Retorna:
--
-- * Una String con la próxima adivinación.
interpret :: Rose MinimaxNode -> String
interpret tree =
    -- Toma el mínimo de los hijos del primer nivel
    value $ minimum (map getNodeVal childs)
        where
            -- Puntúa el árbol
            tree' = scoreTree tree

            -- Toma los hijos del primer nivel
            Node _ childs = tree'

-- | Puntúa un árbol minimax dado, dado el padre.
-- 
-- Retorna:
--
-- * El árbol minimax puntuado desde la raíz.
scoreTree :: Rose MinimaxNode -> Rose MinimaxNode
scoreTree (Node val childs) =
    let childs' = map scoreNode childs
    in Node val childs'

-- | Puntúa un nodo de un árbol minimax dado.
--
-- Incrementa recursivamente a cada nodo la puntuación de sus nodos hijos.
-- 
-- Retorna:
--
-- * El nodo puntuado.
scoreNode :: Rose MinimaxNode -> Rose MinimaxNode

-- Si es una hoja, se deja igual (es un último nivel)
scoreNode leaf@(Leaf val) = leaf

-- Si es una rama, se puntúan primero los hijos y se incrementa al nodo
-- actual la puntuación de sus hijos.
scoreNode tree@(Node val childs) =
    -- Crea función recursiva de puntuación
    let f l@(Leaf val) = l
        f anotherTree = scoreNode anotherTree
        childs' = map f childs

        -- Extrae información del nodo viejo
        (MinimaxNode v wS sS sc) = val

        -- Incrementa al nodo las puntuaciones de sus hijos
        val' = MinimaxNode v wS sS $ sc + sum (map (score . getNodeVal) childs')
    in Node val' childs'

-- $doc
-- == Generación de la adivinación

-- | Genera una adivinación para la siguiente ronda.
--
-- Recibe la última adivinación realizada, la string de score del usuario,
-- el conjunto de adivinaciones posibles y el conjunto de scores posibles.
-- 
-- Retorna:
--
-- * Left mensaje si la string de puntuación no es válida o es imposible
--   el score dado, con el mensaje de error en el primer caso, o "T" en
--   el segundo.
-- * Right (nextGuess, guessSet', scoreSet') en caso contrario, con la
--   adivinación generada y los conjuntos de guess y score filtrados para
--   una siguiente ronda.
guessNext :: String    -- ^ Palabra adivinada
          -> String    -- ^ Hint del usuario
          -> Set Guess -- ^ Conjunto de palabras restantes
          -> Set Hint -- ^ Conjunto de scores restantes
          -> Either String (String, Set Guess, Set Hint) -- ^ Contexto de la siguiente adivinación
guessNext guess score guessSet scoreSet =
    -- Verifica la string de score del usuario
    let verifScore = validateScore score

        -- Filtra los conjuntos
        scoreSet' = filterScoreSet scoreSet score
        guessSet' = filterGuessSet guessSet guess score

        -- Genera el nodo padre del árbol y el árbol minimax a partir de él
        parentNode = MinimaxNode score guessSet' scoreSet' 0
        minimaxTree = Node parentNode $ generateMinLevel parentNode 1

        -- Interpreta el árbol minimax y obtiene la adivinación
        nextGuess = interpret minimaxTree
    in
        maybe
        -- Si es válida la string de calificación
        ( if Set.null guessSet'
            -- Si no es posible el score dado, tramposo
            then Left "T"
            else Right (nextGuess, guessSet', scoreSet') )
        -- Si es inválida, retorna el mensaje de error
        Left verifScore