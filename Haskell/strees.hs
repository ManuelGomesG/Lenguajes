{-
Manuel Gomes
11-10375
Implementaciones de unzip  usando recursión directa, listas por comprensión foldr y map
-}

import Data.List
import Data.Maybe

data SuffixTree = Leaf Int
	|	Node [(String,SuffixTree)]
	deriving (Eq,Ord,Show)



t1 :: SuffixTree
t1 = Node [("banana", Leaf 0),
	("a", Node [("na", Node [("na", Leaf 1),
							 ("", Leaf 3)]),
				("", Leaf 5)]),
	("na", Node [("na", Leaf 2),
				 ("", Leaf 4)])]



isPrefix :: String -> String -> Bool
isPrefix [] _         =  True
isPrefix _  []        =  False
isPrefix (p:ps) (s:ss)=  p == s && isPrefix ps ss


removePrefix :: String -> String -> String
removePrefix p s 
				| p == ""		= s  -- Porque "" es prefijo de cualquier string.
				| isPrefix p s 	= removePrefix (tail(p)) (tail(s))
				| otherwise 	= s



-- Revisar
suffixes :: [a] -> [[a]]
suffixes []	= [[]]		 --Para que no de error de lista vacía.
suffixes s 	= s:(suffixes(tail(s)))


-- Usando las funciones isPrefix y suffixes, escriba la función
isSubstring :: String -> String -> Bool
isSubstring s1 s2 = or $fmap (isPrefix s1) bs
						where bs = suffixes s2



--Usando las funciones isPrefix y suffixes, escriba la función
--que produce los índices de todas las ocurrencias de s1 en s2 usando búsqueda
--directa inocente.
findSubstrings :: String -> String -> [Int]
findSubstrings s1 s2 = elemIndices True $fmap (isPrefix s1) bs
						where bs = suffixes s2



--(1.5 puntos) Escriba la función
--que produce los valores almacenados en las hojas del árbol. El orden de los
--valores no es importante – y no gana nada si los ordena.
getIndices :: SuffixTree -> [Int]
getIndices (Leaf int) = [int]
getIndices Node ts	  = Node $map getIndices branches 
							where branches = snd $ unzip ts