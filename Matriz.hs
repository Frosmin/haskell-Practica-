

sobrecargaRampa xss = rampa xss 1

rampa [] _ = []
rampa (x:xss) a = (tomar x a) : rampa xss (a+1)

tomar [] _ = []
tomar _ 0 = []
tomar (x:xs) i = x : tomar xs (i-1) 

rmp = [[1,2,3],[4,5,6],[7,8,9]]


dobrecargaDiagonal xss = diagonal xss 1

diagonal [] _ = 0 
diagonal (x:xss) i = elemento x i + diagonal xss (i+1)

elemento [] _ = -1
elemento (x:xs) 1 = x
elemento (x:xs) n = elemento xs (n-1)

columna [] _ = []
columna  (x:xss) c = elemento x c : columna xss c

columnaDelPrimero [] = []
columnaDelPrimero ((x:xs):xss) = x : columnaDelPrimero xss



transpuesta  xss  i t | i == t = []
transpuesta xss i t = columna xss i : transpuesta xss (i+1) t


sobrecargaTanspuesta xss = transpuesta xss 1 ((length xss)+1)
matri = [[1,2],[3,4]]


multi2Listas [] [] = 0
multi2Listas (x:xs) (y:ys) = x*y + multi2Listas xs ys

multiM [] _ = []
multiM (x:xss) ys = multi2Listas x ys : multiM xss ys


multiplicacionMatrices _ [] = []
multiplicacionMatrices xss (y:yss) = multiM xss y : multiplicacionMatrices xss yss

sobrecargaMulti xss yss = multiplicacionMatrices (sobrecargaTanspuesta xss) yss


m1 = [[1,2],[3,4]] 
multiplicar3 xss yss zss = sobrecargaMulti (sobrecargaMulti xss yss) zss

multiplicar4 xss yss zss dss = sobrecargaMulti (sobrecargaMulti (sobrecargaMulti xss yss) zss) dss