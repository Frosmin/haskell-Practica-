
f1 :: Bool -> Int -> Int -> Int
f1 x y z = if x then y+10 else z

f2 :: Int -> Int -> Char -> Int
f2 x y z = if z=='s' then 2*x else y


f3 :: (Int -> Bool) -> (Char -> Bool) -> (Bool -> Bool) -> Bool
f3 x y z = (x 2) && (y 'a') && (z True)



f6 :: p -> p
f6 x = x

f7 :: (tx -> ty) -> tx -> ty
f7 x y = x y

f8 :: (x -> y )-> (z -> x) -> z -> y
f8 x y z = x (y z)

f9 :: (y->z->r)-> y-> z ->r
f9 x y z = (x y) z

f10 :: (t1 -> t2) -> (t3 -> t1) -> (t4 -> t3) -> t4 -> t2
f10 x y z w= x (y (z w))

sumar3 x y z = x + y + z

cuatroytres = sumar3  1 1


suma a b = a+b

aplicar :: (Int -> Int) -> Int -> Int
aplicar x y  = x y

aplicar3 :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
aplicar3 x y z = x y z

por :: Int -> Int -> Int
por = (*)



mas = (+)

fun2 x = x



xd::(ty -> ta) -> ty -> ta
xd a b = a b


h2 :: Bool -> (Bool -> (Int -> Int))
h2 x y z = z

h4 :: (Bool -> Bool) -> Int -> Int
h4 x y = y




h44 x y = y


devuleveTercero xs = xs !! 2

--Definir una función que reciba una lista de listas de funciones y un elemento y aplique la 1ra función
--de la primera lista al elemento

funcionDefunciones xs n  = head (head xs) n


suma1 n = 1+n

suma2 n = 2+n


--Definir una función que reciba una lista de listas y devuelva el 5to. Elemento de la 3ra. lista.
listaListas xs = (xs!!2)!!4


--Definir una función que reciba una lista de listas de listas y devuelva el 3er. elemento de la 4ta. Lista de la 2da. lista
lista3 xs = ((xs!!1)!!3)!!2

--Definir una función que verifique si una lista esta ordenada de acuerdo a una función de orden.
orden xs f = and (zipWith f (init xs) (tail xs))

--factorial
factorial n = foldr  1 [1..n]


--Definir una función que compare 2 listas y devuelva True si las listas son iguales

igual xs ys = and (zipWith (==) xs ys)


--Definir una función que verifique si una lista de listas podría ser considerada una matriz

matriz xs = (<=) (length (tail xs)) 1

--Definir una función que reciba un número y una lista y devuelva el elemento de la lista que esta en la posición n
lsita2 xs n  = xs!!(n-1)

--Definir las funciones length, filter, zip utilizando las otras funciones

--length
tamaño xs = sum (zipWith (/) xs xs)
--zip
zipcito xs ys  = zipWith (,) xs ys


filtercito xs f  = [x|x <- xs, f x]


--Definir una función que reciba una matriz y devuelva su transpuesta


--[[1,2,3],
--[4,5,6],
--[7,8,9]]


getCol mss c = map (!!c) mss    --saca los valore deseados de una matriz (LA SACA LA COLUMNA DESEADA)

ultimaCol matriz = length (head matriz) -1

transpuesta mss = map(getCol mss)[0..ultimaCol mss]

--transponer matriz = (map head matriz) : transponer (map tail matriz)



