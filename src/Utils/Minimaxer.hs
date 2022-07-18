{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-|
Módulo      : Utils.Minimaxer
Descripción : Motor de minimax para implementación de Wordle.
    en Haskell.
Copyright   : (c) Christopher Gómez, 2022
    Nestor Javier, 2022
Licencia    : GPL-3
-}
module Utils.Minimaxer where

import Data.Set ( Set, notMember )
import qualified Data.Set as Set
import Data.List ( foldl' )
import Data.Char ( isAlpha, toUpper )
import Data.Either ( isLeft, fromLeft, fromRight )
import Data.Map ( fromListWith, toList )

-- ============== DEFINICIONES DE TIPOS DE DATOS ==============

-- | Tipo de dato para representar una palabra.
data Guess = Guess String Int
    deriving (Show, Eq)

instance Ord Guess where
    compare (Guess w1 s1) (Guess w2 s2) = compare (s1, w1) (s2, w2)

-- | Obtiene la String de un Guess.
guessString :: Guess -> String
guessString (Guess word _) = word

-- | Tipo de dato para representar una string de calificación del usuario.
data Score = Score String Int
    deriving (Show, Eq)

instance Ord Score where
    compare (Score w1 s1) (Score w2 s2) = compare (s2, w2) (s1, w1)

-- | Obtiene la String de un Score.
scoreString :: Score -> String
scoreString (Score string _) = string

-- =========== FUNCIONES PARA MANEJAR TIPOS DE DATOS ===========

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
            | otherwise        =  0

-- | Retorna un Score dado una string con la calificación del usuario,
-- puntuada de acuerdo al algoritmo de Minimax.
--
-- Usa enteros para salvar la eficiencia que puede perderse al manipular
-- números de punto flotante.
scoreFromString :: String -> Score
scoreFromString string = Score string (sum $ map qualifyChar string)
    where qualifyChar 'T'  = 0
          qualifyChar 'V'  = 1
          qualifyChar '-'  = 2
          qualifyChar  c   = error "Caracter inválido" -- Nunca debería ocurrir

-- =============== DEFINICIONES DE CONJUNTOS ==========================

-- | Crea el conjunto inicial con todas las Guess del juego, dadas estas
-- en un conjunto de String.
initWordSet :: Set String -> Set Guess
initWordSet = Set.map guessFromString

-- | Conjunto con todas las combinaciones de Scores posibles.
initScoreSet :: Set Score
initScoreSet = Set.fromList [ scoreFromString str | str <- scoreStrings ]
    where
        x = "-VT"
        scoreStrings = [[a, b, c, d, e] | a<-x, b<-x, c<-x, d<-x, e<-x]

-- ============== MISCELÁNEOS ==============

-- | Verifica si una string de calificación del usuario es válida.
-- 
-- Retorna:
--
-- * Left mensaje si la string no es válida, con el mensaje de error.
-- * Right string si la string es válida, con la misma string.
-- 
-- Una palabra es válida si:
--
-- * Tiene 5 caracteres.
-- * Solo contiene los caracteres '-', 'V' y 'T'.
validateScore :: String -> Either String String
validateScore score
    | length score /= 5
        = Left "La calificación debe contener 5 caracteres."
    | any (`notElem` "TV-") score
        = Left  "La calificación solo puede contener los caracteres -, V y T."
    | otherwise = Right score

-- | Cuenta el número de ocurrencias de un elemento en una lista.
count :: (Eq a) => a -> [a] -> Int
count c = length . filter (==c)

-- | Retorna un booleano indicando si un Score es válido dada la string
-- de adivinación, el conjunto de adivinaciones posibles y el Score.
isValidScore :: String ->  Set Guess -> Score -> Bool
isValidScore guess guessSet score =
    -- Es válido si el conjunto resultante de filtrar las posibles
    -- adivinaciones usando ese Score tiene al menos un elemento
    not $ Set.null (filterGuessSet guessSet guess $ scoreString score)

-- =========== FILTRADO DEL CONJUNTO DE PALABRAS ===========

-- | Filtra el conjunto de palabras posibles según las Strings dadas.
--
-- Recibe el conjunto inicial, la palabra adivinada y la puntuación
-- del usuario.
filterGuessSet :: Set Guess -> String -> String -> Set Guess
filterGuessSet words guess score =
    let fils = getFilters guess score
        f = unifyFilters fils
    in Set.filter (\(Guess str _) -> f str) words


-- | Recibe un Guess y un Score del usuario y retorna una lista de filtros
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

-- =========== FILTRADO DEL CONJUNTO DE SCORES ===========

-- | Filtra el conjunto de Scores posibles tras la Score dada.
--
-- Recibe el conjunto inicial y la puntuación del usuario.
filterScoreSet :: Set Score -> String -> Set Score
filterScoreSet scores str =
    let f = unifyFilters $ freqFilter str : posFilters' str 0  in
        Set.filter (\(Score sc _) -> f sc) scores

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

-- =========== AGENTE MINIMAX ===========

-- =========== DEFINICIÓN DE ÁRBOL Y NODOS ===========

-- | Tipo de dato para representar un árbol con cantidad no acotada
-- de nodos hijos.
data Rose a = Leaf a | Node a [Rose a]
    deriving Show

-- | Tipo de dato para que representa un nodo del agente Minimax.
data MinimaxNode = MinimaxNode {
    value   :: String, -- ^ Valor del nodo (una palabra o una puntuación).
    wordSet :: Set Guess, -- ^ Conjunto de Guess válidos al momento.
    scoreSet :: Set Score, -- ^ Conjunto de Scores válidos al momento.
    score    :: Int, -- ^ Puntuación minimax del nodo.
    depth    :: Int -- ^ Profundidad del nodo en el árbol.
} deriving (Show, Eq)

-- Nodos se ordenan según su puntuación
instance Ord MinimaxNode where
    compare a b = compare (score a) (score b)

-- | Obtiene el valor de un nodo de Rose.
getNodeVal :: Rose a -> a
getNodeVal (Leaf val  ) = val
getNodeVal (Node val _) = val

-- =========== GENERACIÓN E INTERPRETACIÓN DEL ÁRBOL MINIMAX ===========

-- | Genera un nivel de minimización de árbol de Minimax, dado un nodo padre.
-- 
-- Un nivel de minimización es una lista de nodos hijo, donde cada nodo
-- contiene las 10 palabras con menor puntuación que se pueden adivinar
-- dada la información del padre.
generateMinLevel :: MinimaxNode -> [Rose MinimaxNode]
generateMinLevel node =
    -- Extrae la información del padre
    let (MinimaxNode _ wSet sSet sc level) = node
        -- Toma las 10 palabras con menor puntuación
        words = Set.toList $ Set.take 10 wSet

        -- Genera con esas palabras lista de MinimaxNode
        nodeList = map
            (\(Guess w sc) -> MinimaxNode w wSet sSet sc (level+1)) words

    -- Mapea cada nodo de la lista a un Rose rama
    in map (\n -> Node n $ generateMaxLevel n) nodeList

-- | Genera un nivel de maximización de árbol de Minimax, dado un nodo padre.
-- 
-- Un nivel de maximización es una lista de nodos hijo, donde cada nodo
-- contiene las 10 strings de score con mayor puntuación que son posibles
-- respuestas del usuario, dada la información del padre.
generateMaxLevel :: MinimaxNode -> [Rose MinimaxNode]
generateMaxLevel node =
    -- Extrae la información del padre
    let (MinimaxNode val wSet sSet _ level) = node
        -- Toma los 10 Scores válidos con menor puntuación
        scores = Set.take 10 $ Set.filter (isValidScore val wSet) sSet

        -- Genera con esos Scores lista de MinimaxNode
        nodeList = map
            (\(Score str sc) -> MinimaxNode
                str
                (filterGuessSet wSet val str)
                (filterScoreSet sSet str)
                sc
                (level+1))
            (Set.toList scores)

    in
        -- Mapea cada nodo de la lista a un Rose hoja si ya hay 4 niveles de profundidad
        if level + 1 == 4 then map Leaf nodeList

        -- De otra forna, mapea cada nodo de la lista a un Rose rama
        else map (\n -> Node n $ generateMinLevel n) nodeList

-- | Interpreta un árbol minimax dado.
--
-- Incrementa recursivamente a cada nodo la puntuación de sus nodos hijos.
-- Escoje el nodo del primer nivel con menor puntuación.
-- 
-- Retorna:
--
-- * Una String con la próxima adivinación
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
        (MinimaxNode v wS sS sc d) = val

        -- Incrementa al nodo las puntuaciones de sus hijos
        val' = MinimaxNode v wS sS (sc + sum (map (score . getNodeVal) childs')) d
    in Node val' childs'

-- =========== GENERACIÓN DE LA ADIVINACIÓN ===========

-- | Genera una adivinación para la siguiente ronda.
--
-- Recibe la última adivinación realizada, la string de score del usuario,
-- el conjunto de adivinaciones posibles y el conjunto de scores posibles.
-- 
-- Retorna:
--
-- * Left mensaje si la string de adivinación no es válida o es imposible
--   el score dado, con el mensaje de error en el primer caso, o "T" en
--   el segundo.
-- * Right (nextGuess, guessSet', scoreSet') en caso contrario, con la
--   adivinación generada y los conjuntos de guess y score filtrados para
--   una siguiente ronda.
guessNext :: String -- ^ Palabra adivinada
    -> String  -- ^ Score del usuario
    -> Set Guess -- ^ Conjunto de palabras restantes
    -> Set Score  -- ^ Conjunto de scores restantes
    -> Either String (String, Set Guess, Set Score) -- ^  Contexto de la siguiente adivinación
guessNext guess score guessSet scoreSet =
    -- Verifica la string de score del usuario
    let verifScore = validateScore score

        -- Filtra los conjuntos
        scoreSet' = filterScoreSet scoreSet score
        guessSet' = filterGuessSet guessSet guess score

        -- Genera el nodo padre del árbol y el árbol minimax a partir de él
        parentNode = MinimaxNode score guessSet' scoreSet' 0 0
        minimaxTree = Node parentNode $ generateMinLevel parentNode

        -- Interpreta el árbol minimax y obtiene la adivinación
        nextGuess = interpret minimaxTree
    in
        -- Si es inválida la string de calificación, retorna el mensaje de error
        if isLeft verifScore
            then Left $ fromLeft "" verifScore

            -- Si es imposible la respuesta del usuario, retorna "T" de tramposo
            else if Set.null guessSet'
                then Left "T"
                else Right (nextGuess, guessSet', scoreSet')