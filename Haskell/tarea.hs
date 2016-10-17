{-
Manuel Gomes
11-10375
Implementaciones de unzip  usando recursión directa, listas por comprensión foldr y map
-}



unzipF :: [(a, b)] -> ([a], [b])
unzipF   =  foldr (\(x,y) ~(xs,ys) -> (x:xs,y:ys)) ([],[])



unzipR ::  [(a, b)] -> ([a], [b])
unzipR [] 			= ([],[])
unzipR ((a,b):xs) 	= (a : (fst rest) , b : (snd rest)) 
	where rest 		= unzipR(xs)



unzipM ::  [(a, b)] -> ([a], [b])
unzipM []	= ([],[])
unzipM xs	= (map fst xs, map snd xs)



unzipL :: [(a, b)] -> ([a], [b])
unzipL list	= ([x | (x,_) <- list], [y | (_,y) <- list])  