import Text.XHtml (yellow)




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





getCol mss c = map (!!c) mss    
getFila mss c =map take((c+1) mss)

ultimaCol matriz = length (head matriz) -1

transpuesta mss = map(getCol mss)[0..ultimaCol mss]





mul xs ys = sum (zipWith (*) xs ys)

--multiplicaion de matrizes 

multi mat1 mat2 = mul(getFila mat1 0) (getCol mat2 0)

multiFila mat fs = map(mul fs) mat

multiTodasFilas mat1 mat2 = map(multiFila mat2) mat1

fm mat1 mat2 = multiTodasFilas mat1(transpuesta mat2)




--Definir una función que reciba una lista de números y devuelva todos los números pares
lista xs = filter (even) xs

--Definir una función que reciba una lista de listas y devuelva una lista de sus longitudes.
funci xs = map length xs

--Definir una función que reciba una lista de listas y devuelva solo aquellas cuya longitud sea par.

par xss = filter(even) (map(length) xss)

--Definir una función que reciba una lista de listas de números y borre todos los números pares de estas listas
borar xss = map (filter (odd)) xss

--Definir una función que reciba una lista de listas y devuelva una lista formada por los penúltimos elementos de las listas

pen2 xss = map penultimo xss
    where penultimo xs = xs !! (length xs -2)

penultimos xss = map penlul xss
    where penlul xs = head (drop(length xs -2) xs)


--Definir una función que reciba un número y devuelva una lista con los posibles divisores del número.

divi n = map (n/) [0..n]

divisores n = filter ((==0).(mod n))[1..n]

--Definir la función zipWith en terminos de zip
zipw f xs ys = zip(xs ys) f -- revisar

--(map f).(map g) xs = map p xs


--Una función que reciba una lista y devuelva la productoria de sus elementos
productoria xs = foldr (*) 1 xs

--map en fold
mapf f xs = foldr (aplicador f) [] xs
    where 
        aplicador f x xs = f x : xs
       
        
map1 f = foldr (\x xs -> f x : xs) []


--Una función que reciba una lista de dígitos y devuelva el Nro. que se forma al juntarlos.



--Aplicando Listas por Comprensión, definir:
--1. filter

mapComprencion f xs = [f x | x <- xs]

mayor x y =  if x > y then x else y

mayor2 n1 n2 n3 n4   
     | n1 > n2 && n2 > n3 && n3 > n4 = n1
     | n2 > n1 && n2 > n3 && n2 > n4 = n2
     | n3 > n4 && n3 > n2 && n3 > n1 = n3
    |otherwise = n4



diasTranscurridos (d1,m1,a1) (d2,m2,a2)
            |d1 > d2 = d1-d2 + (mesesTranscurridos(d1,m1,a1) (d2,m2,a2)) *30
            |otherwise = d2 -d1 + (mesesTranscurridos(d1,m1,a1) (d2,m2,a2)) *30 


mesesTranscurridos (d1,m1,a1) (d2,m2,a2)
             |m1 > m2 = m1-m2 + (anoTranscurridos(d1,m1,a1) (d2,m2,a2))*12 
             |otherwise = m2-m1 + (anoTranscurridos(d1,m1,a1) (d2,m2,a2))*12 


anoTranscurridos (d1,m1,a1) (d2,m2,a2)
                | a1 > a2 = a1-a2
                |otherwise = a2-a1


diaMesAno (d1,m1,a1) (d2,m2,a2) = (diasTranscurridos(d1,m1,a1) (d2,m2,a2),mesesTranscurridos (d1,m1,a1) (d2,m2,a2),anoTranscurridos (d1,m1,a1) (d2,m2,a2))



--matriz ordenada 


lisOrdenada xs f = zipWith f xs  (drop 1 xs) 

--diagnal de una matriz 
diagonalMatriz mss = [(mss!!i)!!i|  i<-[0..(length mss) -1] ]

colMatriz mss c = [fs!!c| fs <- mss ]

cuadrados = [x^2 | x <- [1, 2, 3, 4, 5]]

suma11 xs = [y+1| y <- xs]

--filter
filterC f xs = [y|y <- xs, f y == True ]


-- concat con listas por comprencion 

--concatC xss 

tama xs = sum [1| _ <- xs ]

--Una función que reciba una cadena y la encripte, cambiando las vocales por los caracteres correspondientes a los dígitos 1,2,3,4,5 respectivamente.

encrita cad = [encriptarCaracter x | x <- cad]
encriptarCaracter c
      | c == 'a' || c == 'A' = '1'
      | c == 'e' || c == 'E' = '2'
      | c == 'i' || c == 'I' = '3'
      | c == 'o' || c == 'O' = '4'
      | c == 'u' || c == 'U' = '5'
      | otherwise = c

--contar caracteres 
contraCar cad = sum [2| _ <- cad]

--Una función que realice el producto cartesiano de dos conjuntos
carteciano xs ys = [(x,y)| x<-xs, y<-ys]

--Una función que reciba un conjunto y un elemento y devuelva True si el element pertenece al conjunto, falso en otro caso.
pertenece xs x = [y|y <- xs, y == x eliminador ]

eliminador c xs = filter (/= c) xs 

--Practicando tipos
f36 ::Bool -> Int -> Int -> Int
f36 x y z = if x then y+10 else z


f2222 :: Int -> Int -> Char -> Int 
f2222 x y z = if z=='s' then 2*x else y



f333 x y z = ( x 2) && (y 'a') && (z True)


f4 :: (Integer -> Bool) -> p -> Integer -> Integer -> Integer
f4=(\x -> \y -> \z -> \w -> if x 2 then z else w+10 )


f777::(y->r)->y->r
f777 x y = x y



--f88:: (z->r) -> (z->r) -> z 
f88 x y z = x (y z)


f x y z = x (y z)

sumaLamda = \x-> \y -> x+y


orden1 xs f = and( zipWith f (init xs) (tail xs))


m f xs= [f(x)|x<-xs]


fi f xs = [x|x<- xs, f x == True]



esPrimo n = if length(primo n) <= 2 then True else False 
    where primo n = [x|x<-[1..n],mod n x == 0 ]


juan xs = [x+y| x<-xs, y<-[1..length(xs)] ]


f111 x y z = x (y z)(y z)



f221 e x y z w = if x&&(y x) then z else w z
        where w a |a = e
                  |otherwise = z