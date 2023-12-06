import GHC.Exts.Heap (ClosureType(TREC_CHUNK))

f1 = [x*2 | x <- [1..10], x*2 >= 12] 
mifilter f xs = [x | x <- xs, f x]   

miMap f xs = [f x| x <- xs ]


-- hacer la funcion length con comprension de listas
milength xs = sum [1 | x <- xs]

miconcat xss = [x | xs <- xss, x <- xs]




-- | Calcula el producto de dos listas filtrando los elementos que cumplen las siguientes condiciones:
--   - El elemento de la primera lista es mayor que 1.
--   - El elemento de la segunda lista es menor que 4.
--
-- >>> producto [1,2,3] [2,3,4]
-- [(2,2),(2,3),(3,2),(3,3)]
--
-- >>> producto [1,2,3] [4,5,6]
-- []
--
producto :: [Int] -> [Int] -> [(Int, Int)]
producto xs ys = [(x,y) | x <- xs, y <- ys, x>1, y<4]


valorVocal c | c == 'a' = '1'
             | c == 'e' = '2'
             | c == 'i' = '3'
             | c == 'o' = '4'
             | c == 'u' = '5'
             | otherwise = c

--Una función que reciba una cadena y la encripte, cambiando las vocales por los
--caracteres correspondientes a los dígitos 1,2,3,4,5 respectivamente.

encriptar xs = [valorVocal x | x <- xs]

pertenece n xs = [True | x <- xs, n == x]
perte xs n | pertenece n xs == [] = False
           | otherwise = True



mat1 = [[1,2,3],[4,5,6],[7,8,9,8]]

sacarColum n xss  = [x!!(n-1) | x <- xss]


trnaspuesta xss = [sacarColum x xss|x <- [1..length xss]]


-- funcion que reciba una matriz y devuelva la diagonal principal de la matriz.

diagonal xss = sum[x!!(n-1) | (x,n) <- zip xss [1..length xss]]

transp xss = [[y]!!x| x <- [0..(length xss +1)], y <- xss]

-- funcion que reciba una matriz y devuelva la diagonal secundarria de la matriz sin usar zip.




nueroFilasCols xss = if length( head xss) == length xss  then True else False 


juan [] _ = True
juan (x:xss) yss  |length x == length yss = juan xss yss
                  |otherwise = False



primo1 n = if sumaTrue(primo n) == 2 then True else False

sumaTrue xs = sum[1 | x <- xs ]
primo n  =[True | x <- [1..n], (mod n x) == 0]



casos 'a' = 'e'
casos 'e' = 'i'
casos 'i' = 'o'
casos 'o' = 'u'
casos 'u' = 'a'
casos _ = 'n'