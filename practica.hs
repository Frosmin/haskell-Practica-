--Recursividad
--datas 
--listas
--arboles
--foldr

data Arbolde3 a b = Hoja a | Rama b (Arbolde3 a b) (Arbolde3 a b) (Arbolde3 a b)
    deriving Show

data Lista a =  Vacia | Add a (Lista a)
    deriving Show



data Natural = Cero | Sig Natural 
    deriving Show

instance Eq Natural where
    Cero == Cero = True
    Sig n1 == Sig n2 = n1 == n2
    _ == _ = False

data Arbol3Hojas a = Hoji a | Rami (Arbol3Hojas a) (Arbol3Hojas a) (Arbol3Hojas a) 
    deriving Show

data Arbol2o3 a = Hojo a | Rama2 (Arbol2o3 a) (Arbol2o3 a)
                         | Rama3 (Arbol2o3 a) (Arbol2o3 a)


lengthLista Vacia = 0
lengthLista (Add _ xs) = 1 + lengthLista xs  

concatenarListas Vacia lista2 = lista2
concatenarListas (Add x xs) lista2 = Add x (concatenarListas xs lista2)



arbolTupla2 arb = (listaInt arb, listaChar arb)

listaInt (Hoja x) = Add x Vacia
listaInt (Rama _ ai am ad) = concatenarListas (concatenarListas (listaInt ai)  (listaInt am))  (listaInt ad) 

listaChar  (Hoja _) = Vacia
listaChar (Rama x ai am ad) = concatenarListas (concatenarListas (concatenarListas (Add x Vacia)  (listaChar ai))  (listaChar am) ) (listaChar ad) 


arbolEjemplo = Rama "Root" (Hoja "A") (Hoja "B") (Rama "Inner" (Hoja "C") (Hoja "D") (Hoja "E"))
ejemploArbol =
    Rama 'A'
        (Rama 'B' (Hoja 1) (Hoja 2) (Hoja 3))
        (Hoja 4)
        (Rama 'C' (Hoja 5) (Hoja 6) (Hoja 7))


{-mayorArbol :: Arbolde3 Int Int -> Int
mayorArbol (Hoja x) mayor | mayor < x = x 
                          |otherwise = mayor
mayorArbol (Rama y ai am ad) mayor | y > (mayorArbol ai) && y > (mayorArbol am) && y > (mayorArbol ad) = y 
                                   | (mayorArbol ai) > (mayorArbol am) && (mayorArbol ai) > (mayorArbol ad) = (mayorArbol ai)
                                   | (mayorArbol am) > (mayorArbol ad) = (mayorArbol am)
                                   | (mayorArbol ad) > mayor = (mayorArbol ad)
                                   |otherwise = mayor-}

mayorArbol (Hoja x) = x
mayorArbol (Rama y ai am ad) = max  (max y (mayorArbol ai))  (max (mayorArbol am) (mayorArbol ad))




ejemploArbolInt =
    Rami
        (Rami  (Hoji 3) (Hoji 7) (Hoji 2))
        (Hoji 10)
        (Rami (Hoji 8) (Hoji 18) (Hoji 20))

--8. Definir una función que reciba dos listas xs, ys (del tipo Lista a) y devuelva cuantas veces ocurre xs en ys.

sumaOcurecncias _ Vacia = 0
sumaOcurecncias xs (Add y ys) |ocurecias xs (Add y ys) = 1 + sumaOcurecncias xs ys
                              |otherwise = sumaOcurecncias xs ys 

ocurecias Vacia _ = True
ocurecias _ Vacia = False
ocurecias (Add x xs) (Add y ys) | x == y = ocurecias xs ys 
                                | otherwise = False


listaPrueba1 = (Add 2 (Add 2 Vacia))
listaPrueba2 = (Add 2 (Add 2 (Add 1 (Add 2 Vacia)))) 
                

cuantasHojas (Hoji _) = 1
cuantasHojas (Rami ai am ad) = (cuantasHojas ai) + (cuantasHojas am) + (cuantasHojas ad)

nodosNoterminales (Hoji _) = 0
nodosNoterminales (Rami ai am ad) = 1 + (nodosNoterminales ai) + (nodosNoterminales am) + (nodosNoterminales ad)

suatoriaHojas (Hoji x) = x
suatoriaHojas (Rami ai am ad) = (suatoriaHojas ai) + (suatoriaHojas am) + (suatoriaHojas ad)

instance Eq a => Eq (Arbol3Hojas a) where
    Hoji x == Hoji y = x == y
    Rami  ai am ad == Rami ai2 am2 ad2 = ai == ai2 && am == am2 && ad == ad2  

instance Eq a => Eq (Lista a) where 
    Vacia == Vacia = True
    Add x xs == Add y ys = x == y && xs == ys 


ejemploArbolInt2 =
    Rami
        (Rami  (Hoji 3) (Hoji 7) (Hoji 2))
        (Hoji 10)
        (Rami (Hoji 8) (Hoji 18) (Hoji 20))


ejemploArbolInt3 =
    Rami
        (Rami  (Hoji 3) (Hoji 7) (Hoji 2))
        (Hoji 10)
        (Rami (Hoji 10) (Hoji 18) (Hoji 20))

totalHoja (Hoja _) = 1 
totalHoja (Rama _ ai am ad) = (totalHoja ai) + (totalHoja am) + ( totalHoja ad)

totalNoterminales (Hoja _ ) = 0
totalNoterminales (Rama _ ai am ad) = 1 +(totalNoterminales ai) + (totalNoterminales am) + (totalNoterminales ad)

sumhojas (Hoja x) = x
sumhojas (Rama _ ai am ad) = (sumhojas ai) + (sumhojas am) + (sumhojas ad)

sumaNodosNoTermianles (Hoja _) = 0
sumaNodosNoTermianles (Rama x ai am ad) = x +  (sumhojas ai) + (sumhojas am) + (sumhojas ad)

instance (Eq a ,Eq b) => Eq (Arbolde3 a b) where
    Hoja x1 == Hoja x2 = x1 == x2
    Rama x1 ai1 am1 ad1 == Rama x2 ai2 am2 ad2 = x1 == x2 && ai1 == ai2 && am1 == am2 && ad1 == ad2 

arbolEjemplo3 = Rama "Root" (Hoja "A") (Hoja "B") (Rama "Inner" (Hoja "C") (Hoja "D") (Hoja "E"))

ejemploArbol4 =
    Rama 'A'
        (Rama 'B' (Hoja 1) (Hoja 2) (Hoja 3))
        (Hoja 4)
        (Rama 'C' (Hoja 5) (Hoja 6) (Hoja 7))


ejemploArbol5 =
    Rama 'A'
        (Rama 'a' (Hoja 1) (Hoja 2) (Hoja 3))
        (Hoja 4)
        (Rama 'C' (Hoja 5) (Hoja 6) (Hoja 7))


lista1 = (Add 2 (Add 1 Vacia))
lista2 = (Add 2 (Add 0 Vacia))