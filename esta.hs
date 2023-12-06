import Data.Map.Internal.Debug (balanced)


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


listaOrdenada xs f = listaOrdenada xs f 0
            where listaOrdenada xs f i | i >= (length xs)-1 = True 
                     |otherwise = if f (xs!!i) (xs!!(i+1)) == True then listaOrdenada xs f (i+1) else False



restaLita xs ys = restaLita xs ys [] 0
       where restaLita xs xy bacia i | i >=(length xs) = bacia
                                  |xs!!i == xy!!i = restaLita xs xy bacia (i+1)
                                  | otherwise = restaLita xs xy (xs!!i : bacia) (i+1)  

existe xs n = existe xs n 0
    where existe xs n i | i >= length xs = False
              |xs!!i == n = True
              | i < length xs = existe xs n (i+1)
              

rotarListas xs n 
    | n <= 0 = xs   
    |otherwise =  rotarListas (init( (xs!!(length xs -1)) : xs)) (n-1) 
    
    

--Una función que reciba dos listas xs ys y retorne una lista con las posiciones de inicio donde xs aparece en ys. Por ejemplo:
--posiciones[1,1][1,1,1,2,1,1,2]=>[0,1,4]

posiciones xs ys i i2 nueva
        | (length ys -1) == i = nueva
        | (xs!!i2) == (ys!!i) && (xs!!(i2+1)) == (ys!!(i+1)) = posiciones xs ys (i+1) i2 (i : nueva)
        | otherwise =  posiciones xs ys (i+1) i2 nueva



--------------------------------------------Tarea------------------------------------------------------------------------------------
--multi 2 numeros en base a sumas
sumas x y r
    | y == 0 = r
    |otherwise = (sumas x (y-1) (r+x)) 

    --potencia de un numerator

potencia x y = potencia x y 1
    where potencia x y r 
                 | y == 0 = r
                 |otherwise = (potencia x (y-1) (r*x)) 

--sumatoria de los dijitos de un numero
indicesum x 
            | x < 10 = x
            | otherwise = (mod x 10) + indicesum (quot x 10) 

--cosiente residuo
cosRes x y 
    | x < y = (0,x)
    |otherwise = (cociente + 1, residuo)
     where (cociente, residuo ) =cosRes (x-y) y


sumatoria n 
    | n == 0 = 0
    | otherwise = n + sumatoria(n-1)


-- invertir un numerator
invertirNumero 0 = 0
invertirNumero n = (mod n 10) + invertirNumero (quot n 10)
   

mimap f [] = []
mimap f (x:xs) = f x:(mimap f xs)

mifilter f [] = []
mifilter f (x:xs) = if (f x) == True then x:(mifilter f xs) else mifilter f xs

mizip [] [] = [] 
mizip (x:xs) (y:ys) = (x,y) : mizip xs ys

mizipWith f _ []  = []
mizipWith f [] _  = []
mizipWith f (x:xs) (y:ys) =  (f x y):  (mizipWith f xs ys )

-- length 

milength [] = 0
milength (x:xs) = 1 + milength xs 

--head 
mihead (x:xs) = x

miTail (x:xs) = xs

miTake 0 (x:xs) = []
miTake n (x:xs) = x : miTake (n-1) xs

--drop
miDrop 0 (x:xs) = x:[]
miDrop n (x:xs) = miDrop (n-1) xs

miSum [] = 0
miSum (x:xs) = x + (miSum xs)




-- litas y me devuelve la lista en forma tupla para y devuelve su valor y posicon (a,1) ['a','b']
cadena [] _ = []
cadena (x:xs) i = (x,i) : cadena xs (i+1)

--elemto maximo de una litas 
maximoLita xs = maximoLita xs 1
    where maximoLita [] mayor = [mayor]
          maximoLita (x:xs) mayor = if mayor < x then maximoLita xs x else maximoLita xs mayor   

-- elimianr elemento dado de la lita
eliminarElemento [] e = []
eliminarElemento (x:xs) e = if x == e then  eliminarElemento xs e else x : eliminarElemento xs e


miTakeWhile f [] = []
miTakeWhile f (x:xs) = if f x == True then x : miTakeWhile f xs else []


miDropWhile f []  = []
miDropWhile f (x:xs)  = if f x == True then miDropWhile f xs else x : xs

miPosicion [] p i = -1
miPosicion (x:xs) p i = if p == i then x else miPosicion xs p (i+1)

miConcat [] = []
miConcat (x:xs) = x ++ miConcat xs 


--Practica recursividad
--Definir una función que compare 2 listas y devuelva True si las listas son iguales
compararDosListas [] [] = True
compararDosListas [] _ = False
compararDosListas _ [] = False
compararDosListas (x:xs) (y:ys) = if x == y then compararDosListas xs ys else False 

--2. Definir una función que fusione 2 listas ordenadas en una 3ra. ordenada (sin necesidad de ordenar).

ordenarListas [] ys = ys
ordenarListas (x:xs) ys = ordenarListas xs (meter x ys) 

meter n [] = [n]
meter n (x:xs) = if n < x || n == x then n:x:xs else x: meter n xs

--3. Definir una función que verifique si una lista de listas podría ser considerada una matriz

m1 = [[1,2,3],
      [4,5,6],
      [7,8,9]]

m2 =  [[1,4,3],
       [4,5,6],
       [7,8,9]]

tamañoFila (x:xs) = length x

filasMismoTam [] tam = True
filasMismoTam (x:xs) tam = if length x == tam then filasMismoTam xs tam else False 

contadorFilas [] = 0
contadorFilas (x:xs) = 1+ contadorFilas xs  

esMatriz xs = if (filasMismoTam xs (tamañoFila xs)) == True && contadorFilas xs > 1 then True else False  

--Definir una función que reciba 1 matriz y una función de orden y devuelva True si la matriz esta ordenada de acuerdo a la función de orden.


estaOrdenada [x] f = True
estaOrdenada (x:y:xs) f = if f x y == True then estaOrdenada (y:xs) f else False 

matrizEstaOrdenada [] f = True
matrizEstaOrdenada (x:xs) f = if estaOrdenada x f == True then matrizEstaOrdenada xs f else False

matrizEstaOrdenada2 xs f = if estaOrdenada (miConcat xs) f  == True then True else False 

--Definir una función que reciba una lista de números y devuelva todos los números pares
esPar n = if mod n 2 == 0 then True else False

numerosPares [] = []
numerosPares (x:xs) = if esPar x == True then x : numerosPares xs else numerosPares xs 



--Definir una función que reciba una lista de listas y devuelva solo aquellas cuya longitud sea par.
listaEsPar xs = esPar (length xs) 

matrizPar [] = []
matrizPar (x:xs) = if listaEsPar x == True then x : matrizPar xs else matrizPar xs 

--7. Definir una función que reciba una lista de listas de números y borre todos los números pares de estas listas

borraNumerosParesLita [] = []
borraNumerosParesLita (x:xs) = if esPar x then borraNumerosParesLita xs else x : borraNumerosParesLita xs 

borrarParesMatriz [] = []
borrarParesMatriz (x:xss) = borraNumerosParesLita x : borrarParesMatriz xss


--Definir una función que reciba una lista de listas y devuelva una lista formada por los penúltimos elementos de las listas

penultimoLista (x:xs) = if length xs == 1 then x:[] else penultimoLista xs

penultimosMatriz [] = []
penultimosMatriz (x:xss) = penultimoLista x : penultimosMatriz xss 
union xss = miConcat (penultimosMatriz xss)

-- 9 Definir una función que reciba un número y devuelva una lista con los posibles divisores del número.




--multiplicaion matriz
m3 = [[1,2],
      [3,4]]

m3t = [[1,3],
       [2,4]]

columnas [] i = []
columnas (x:xss) i = x!!i : columnas xss i  

transpu (x:xss) = tran (x:xss) 0 (length x)
tran [] i n = []
tran xss i n =  if i /= n then columnas xss i : tran xss (i+1) n else []


--Definir una función que reciba un número y devuelva una lista con los posibles divisores del número.

divisores3 n i 
    | i == n = [n]
    | mod n i == 0 = i: divisores3 n (i+1) 
    |otherwise = divisores3 n (i+1)


--Definir una función (f xs ys ) que verifique si la lista xs está incluida en la lista ys, devolviendo verdadero o falso según caso.

estaEnlista [] [] = True
estaEnlista (x:xs) [] = False
estaEnlista (x:xs) (y:ys) = if estaEn (x:xs) (y:ys) == True then True else estaEnlista (x:xs) ys

estaEn [] _ = True 
estaEn _ [] = False
estaEn (x:xs) (y:ys) = if x == y then estaEn xs ys else False   

--cuantas veces aparece patron en lista 

cuantasVeces xs [] = 0
cuantasVeces xs (y:ys) = if estaEn xs (y:ys) == True then 1+ cuantasVeces xs ys else cuantasVeces xs ys

--posiciones donde se encuentra el patron en una lista 

posicionEnLista2 xs [] i = []
posicionEnLista2 xs (y:ys) i = if estaEn xs (y:ys) == True then i: posicionEnLista2 xs ys (i+1) else posicionEnLista2 xs ys (i+1) 

fauxParallamar xs ys = posicionEnLista2 xs ys 0

-- tipos 
{-I. Definir los siguientes tipos de datos
 ZonaGeografica que permita representar las 3 zonas geográficas de Bolivia
(valles, llanos y altiplano).
 Departamento que permita representar los 9 departamentos de Bolivia.-}

{-1. Una función que reciba un zona y devuelva un mensaje indicando sus
características
2. Una función que reciba un departamento y devuelva True si pertenece a la zona de
los valles, falso en otro caso.
3. Una función que reciba un departamento y devuelva la zona a la que corresponde
el departamento.
4. Una función que reciba un lista de departamentos y devuelva aquellos que
pertenecen a la zona de los llanos o de los valles.-}

data Zonas = Valles|Llanos|Altiplano deriving Show
data Departamentos = CB|SC|LP|OR|SU|PO|TA|BN|PA deriving (Show,Eq)

instance Eq Zonas where
    Valles == Valles = True
    Llanos == Llanos = True
    Altiplano == Altiplano = True
    _ == _ = False

caracteristicas::Zonas -> String 
caracteristicas zon 
    |zon == Valles ="clima tempaldo"
    |zon == Llanos ="Arido, Planicie"
    |otherwise = "altura , planice"



esllano::Departamentos -> Zonas
esllano dep 
    |dep == CB = Valles
    |dep == LP = Altiplano
    |otherwise = Llanos



dondePertenece [] = []
dondePertenece (x:xs) = if esllano x /= Altiplano then x:dondePertenece xs else dondePertenece xs 


{-II. Sean las siguientes definiciones de tipo:
type Dia = Int
type Mes= Int
type Anio = Int
type Fecha=(Dia,Mes,Anio)
type Periodo=(Fecha,Fecha)
type Nombre=String
type Presidente=(Nombre,Periodo)
el tipo Presidente es un par que representa el Nombre del presidente y el período
de tiempo en que gobernó.
Definir :


1. Una función que reciba un Periodo y devuelva el tiempo transcurrido en años.
2. Una función que reciba un Presidente y devuelva el tiempo total en años que
gobernó.
3. Definir una función que reciba dos presidentes y devuelva aquel que gobernó más
tiempo.
4. Una función que reciba una lista de presidentes y devuelva el nombre del
presidente que menos tiempo gobernó.
5. Una función que reciba una lista de presidentes y devuelva una lista con los
nombres de los presidentes que gobernaron antes del año 1990.
6. Una función que reciba una lista de presidentes y devuelva la cantidad de
presidentes que gobernaron menos de 4 años.

7. Una función que reciba una lista de presidentes y la ordene ascendentemente por
la fecha en que fue presidente.-}


type Dia = Int
type Mes= Int
type Anio = Int
type Fecha=(Dia,Mes,Anio)
type Periodo=(Fecha,Fecha)

type Presidente=(Nombre,Periodo)


tiempoTranscurido:: Periodo -> Int
tiempoTranscurido ((d1,m1,a1),(d2,m2,a2)) = a2 - a1


simon::Presidente
simon = ("simon",((20,10,2003),(20,10,2023)))

samuel::Presidente
samuel = ("samuel",((5,10,2000),(5,10,2023)))

pepe::Presidente
pepe = ("pepe",((10,10,1989),(10,10,1990)))


juan2::Presidente
juan2 = ("juan",((10,10,1800),(10,10,1990)))

tiempoQueGoberno:: Presidente -> Int
tiempoQueGoberno (nombre,periodo) = tiempoTranscurido periodo


cualGovernoMas::Presidente -> Presidente -> Presidente
cualGovernoMas presi1@(nombre1,((d1,m1,a1),(d2,m2,a2))) presi2@(nombre2,((d12,m12,a12),(d22,m22,a22)))  
        |(tiempoQueGoberno (nombre1,((d1,m1,a1),(d2,m2,a2)))) > (tiempoQueGoberno (nombre2,((d12,m12,a12),(d22,m22,a22))) ) = presi1
        |otherwise = presi2




cualGovernoMas2 :: Presidente -> Presidente -> Presidente
cualGovernoMas2 presidente1@(nombre1, periodo1) presidente2@(nombre2, periodo2)
  | tiempoQueGoberno presidente1 > tiempoQueGoberno presidente2 = presidente1
  | otherwise = presidente2

obtenerNombre (nombre,periodo) = nombre

menosTiempoGoberno:: [Presidente] -> String
menosTiempoGoberno [x] = obtenerNombre x
menosTiempoGoberno (x:y:xs) = if cualGovernoMas2 x y == x then menosTiempoGoberno (y:xs)  else menosTiempoGoberno (x:xs) 



queAñoGoberno:: Presidente -> Int
queAñoGoberno (nombre1,((d1,m1,a1),(d2,m2,a2))) = a2


queAñoFueElejido:: Presidente -> Int
queAñoFueElejido (nombre1,((d1,m1,a1),(d2,m2,a2))) = a1

antesde1990 :: [Presidente] -> [Nombre]
antesde1990 [] = []
antesde1990 (x:xs) = if queAñoGoberno x <= 1990 then (obtenerNombre x) : antesde1990 xs else antesde1990 xs 



presisQuegobernaronMenos4 [] = []
presisQuegobernaronMenos4 (x:xs) = if tiempoQueGoberno x < 4 then x: presisQuegobernaronMenos4 xs else presisQuegobernaronMenos4 xs

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    splitInHalf [] = ([], [])
    splitInHalf [x] = ([x], [])
    splitInHalf (x:y:xs) = (x:left, y:right)
      where
        (left, right) = splitInHalf xs

    (left, right) = splitInHalf xs

    merge :: (Ord a) => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

main :: IO ()
main = do
    let lista = [5, 1, 4, 2, 8]
    let listaOrdenada = mergeSort lista
    print listaOrdenada


----------------------------------------------

data Empleado = Docente Nombre Horas SueldoHora Materias
              | Administrativo Nombre Salario Cargo deriving (Show,Eq)
type Nombre = String
type Horas = Int
type SueldoHora = Int
type Salario = Int
type Materias = [String]
type Cargo = String

{-a) Definir una función que reciba dos empleados y devuelva verdad si ambos tienen el
mismo ingreso mensual pero uno es docente y el otro es administrativo.
b) Definir una función que reciba una lista de empleados y devuelva el nombre del
docente que dicta mayor cantidad de materias.
c) Definir una función que reciba una lista de empleados y devuelva el nombre del
empleado que tiene mayor salario mensual.

d) Definir una función que reciba un empleado (e) y una lista ordenada ascendentemente
de acuerdo al ingreso mensual percibido (ls) y que inserte e en ls en la posición que le
corresponde, de modo que la lista resultante siga ordenada.
e) Definir una función que reciba una lista de empleados y devuelva la lista ordenada
ascendentemente por ingresos mensuales del empelado.-}

doce = (Docente "juna" 200 10 ["fisica","compu","mate","labo","dinamica"])
doce2 = (Docente "juna2" 200 10 ["fisica","compu","mate","casa"])
doce3 = (Docente "juna3" 200 30 ["fisica","compu","mate"])
doce4 = (Docente "juna4" 200 10 ["fisica","compu"])

amin = (Administrativo "pepe" 2001 "jefe")

ingresoMensual:: Empleado -> Int
ingresoMensual (Docente _ horas sueldoHora _) = horas * sueldoHora
ingresoMensual (Administrativo _ salario _) = salario

a1 emp1@(Docente _ horas sueldoHora _) emp2@(Administrativo _ salario _) = if ingresoMensual emp1 == ingresoMensual emp2 then True else False

numeroMaterias (Docente _ _ _ materias) = length materias

nombreEmpleado (Administrativo nombre _ _ ) = nombre
nombreEmpleado (Docente nombre _ _ _) = nombre

quienDictaMas [x] = nombreEmpleado x
quienDictaMas (x:y:xs) = if numeroMaterias x > numeroMaterias y then quienDictaMas (x:xs) else quienDictaMas (y:xs)

mayorSalarioMensual [x] = nombreEmpleado x
mayorSalarioMensual (x:y:xs) = if ingresoMensual x > ingresoMensual y then mayorSalarioMensual (x:xs) else mayorSalarioMensual (y:xs)

--tipos recursivos 
data Natural = Cero | Sgte Natural
        deriving Show


entero2Natural:: Int -> Natural
entero2Natural 0 = Cero
entero2Natural n = Sgte(entero2Natural(n-1))

natural2Entero :: Natural -> Int 
natural2Entero Cero = 0
natural2Entero (Sgte n) = 1+(natural2Entero n)


sumaT Cero Cero = Cero
sumaT (Sgte n) Cero = Sgte(sumaT n Cero)
sumaT Cero (Sgte m) = Sgte(sumaT Cero m)
sumaT (Sgte n)(Sgte m) = Sgte(Sgte(sumaT n m))

restaT Cero _ = Cero
restaT n Cero = n
restaT (Sgte n) (Sgte m) = restaT n m


multiT Cero _ = Cero
multiT (Sgte n) m = sumaT m (multiT n m)


expoT _ Cero = Sgte Cero
expoT n (Sgte m) = multiT n (expoT n m)



menorT _ Cero = False
menorT Cero _ = True
menorT (Sgte n) (Sgte m) = menorT n m


mayorT _ Cero = True
mayorT Cero _ = False
mayorT (Sgte n) (Sgte m) = mayorT n m 


esIgualT Cero Cero = True
esIgualT _ Cero = False
esIgualT Cero _ = False
esIgualT (Sgte n) (Sgte m) = esIgualT n m


cocienteT a b
    | menorT a b = Cero  
    | otherwise = Sgte (cocienteT (restaT a b) b)


residuoT a b
    | menorT a  b = a 
    | otherwise = residuoT (restaT a b) b





data Lista a = Vacia | Add a (Lista a)
    deriving Show 

    
mimizip Vacia _ = Vacia
mimizip _ Vacia = Vacia
mimizip (Add x xs) (Add y ys) = Add (x,y) (mimizip xs ys)

transfromarLista Vacia = []
transfromarLista (Add x xs) = x: transfromarLista xs

mimiMap f Vacia = []
mimiMap f (Add x xs) = (f x) : (mimiMap f xs)

mimizipWhit f Vacia _ = []
mimizipWhit f _ Vacia = []
mimizipWhit f (Add x xs) (Add y ys) = (f x y):(mimizipWhit f xs ys) 

l1 = (Add 3(Add 10 Vacia)) 
l2 = (Add 1(Add 4 (Add 3(Add 2 Vacia)))) 

lista2Literal [] = Vacia
lista2Literal (x:xs) = Add x (lista2Literal xs)

masmas Vacia ys = ys
masmas(Add x xs) ys = Add x (masmas xs ys)

matl = Add (Add 1 Vacia) (Add (Add 2 Vacia) Vacia)

concatLista Vacia = Vacia
concatLista (Add x xss) = masmas x (concatLista xss)

miSum3  Vacia = 0
miSum3 (Add x xs) = x + miSum3 xs


miLength Vacia = 0
miLength (Add x xs) =1 + miLength xs

milast3 (Add x xs) = if miLength xs == 0 then x else milast3 xs  

mihead3 (Add x xs) = x


take3 0 _ = Vacia
take3 i (Add x xs) =Add x (take3 (i-1) xs)

mifilter3 f Vacia = Vacia
mifilter3 f (Add x xs) = if f x then Add x (mifilter3 f xs)
                         else mifilter3 f xs


--mitail3 i Vacia = Vacia
--mitail3 i (Add x xs) = if i == 0 then mitail3 (i+1) xs else Add x (mitail3 i xs)



mitail4 0 (Add x xs) = mitail4 1 xs
mitail4 _ (Add x xs) = Add x (mitail4 1 xs)
mitail4 _ Vacia = Vacia


drop3 i Vacia = Vacia
drop3 i (Add x xs) = if i > 0 then drop3 (i-1) xs else  Add x (drop3 (i-1) xs)  



data Arbol a = Hoja a |Rama (Arbol a) (Arbol a) 
        deriving Show
mapA f (Hoja x) = Hoja (f x)
mapA f (Rama ai ad) = Rama (mapA f ai) (mapA f ad)

ar1 =  Rama (Hoja 1) (Rama (Hoja 2) (Hoja 3))


arbol2Lista (Hoja x) = [x]
arbol2Lista (Rama i d) = (arbol2Lista i) ++ (arbol2Lista d)

arbol2Listaa (Hoja x) = Add x Vacia
arbol2Listaa (Rama i d) = masmas (arbol2Listaa i) (arbol2Listaa d)

contarHojas (Hoja x) = 1
contarHojas (Rama i d) = (contarHojas i) + (contarHojas d)

profundidad (Hoja x) = 1
profundidad (Rama i d) = 1 + max (profundidad i)  (profundidad d)


data ArbolBinario a = Vacio | Nodo a (ArbolBinario a) (ArbolBinario a)
            deriving Show

sumaNodos Vacio = 0
sumaNodos (Nodo x i d) = x +(sumaNodos i) + (sumaNodos d)


arbolPrueba =
  Branch 10
    (Branch 5
      (Branch 3 Leaf Leaf)
      (Branch 7 Leaf Leaf)
    )
    (Branch 15
      (Branch 12 Leaf Leaf)
      (Branch 18 Leaf Leaf)
    )



arbolPrueba2 = Branch 200 
    (Branch 150 
        (Branch 100 Leaf Leaf)
        (Branch 170 Leaf Leaf)
    )
    (Branch 280 
        (Branch 250 Leaf Leaf)
        (Branch 300 Leaf Leaf)
    )


mapBB f Vacio = Vacio
mapBB f (Nodo x i d) = Nodo (f x) (mapBB f i) (mapBB f d)



data ArbolB a = Leaf | Branch a (ArbolB a) (ArbolB a)
        deriving Show


list3 Leaf = []
list3 (Branch x ai ad) = list3 ai ++ [x] ++ list3 ad



list4 Leaf = []
list4 (Branch x ai ad) = list4 ad ++ [x] ++ list4 ai


-- practia de datos compuestos 
{-1. Definir una función que reciba una nota y devuelva verdad(True) si esta es de la
UMSS
2. Definir una función que reciba una nota y devuelva su valor sólo en caso que sea de
la Cato o de UMSS. En caso de ser de la Cato, que devuelva el promedio de las dos
notas.
3. Definir una función que reciba una lista de notas y devuelva únicamente las notas de
aprobación. La nota de aprobación para el caso de la Cato es cuando el promedio de
sus valores componentes es mayor a 50 y de la Maestría, cuando el valor es A,B o C.


4. Definir una función que reciba una lista de notas y las devuelva ordenadas (suponer
que una nota de San Simón es mayor a un nota de la Cato y una de Maestría es
mayor que una de la UMSS)
5. Inventar 3 tipos de datos compuestos, para cada tipo inventar 3 funciones que se
apliquen al mismo-}

data Nota = Maestria Char |Cato Int Int|Umss Int
    deriving Show

esUmss:: Nota -> Bool 
esUmss (Umss x) = True
esUmss _ = False 

promedioU (Umss x) = x
promedioU (Cato x y) = div (x+y) 2




listaNotas [] = []
listaNotas (x:xs) = if aprueba x then x : listaNotas xs else listaNotas xs

aprueba (Umss x) = if promedioU (Umss x) > 50 then True else False
aprueba (Cato x y) = if promedioU (Cato x y) > 50 then True else False
aprueba (Maestria c) |c == 'A' = True
                     |c == 'B'= True
                     |c =='C' = True
                     |otherwise = False

listanotas1 = [(Cato 60 51),(Umss 51), (Umss 72),(Maestria 'A'), (Maestria 'F')]


mayorNota:: Nota -> Nota -> Nota
mayorNota _ (Maestria n) = (Maestria n) 
mayorNota (Maestria n) _ = (Maestria n)

mayorNota (Umss x) (Cato y y2) = (Umss x)
mayorNota (Cato y y2) (Umss x)  = (Umss x)

mayorNota (Umss x) (Umss x2)  | x < x2 = (Umss x2)
                              |otherwise = (Umss x)
                              
mayorNota (Cato y1 y11) (Cato y2 y22) | (promedioU (Cato y1 y11)) < (promedioU (Cato y2 y22)) = (Cato y2 y22)
                                      |otherwise = (Cato y1 y11)
{-
mayorNota (Maestria x) (Maestria y) | x == 'A' && y == 'A' = (Maestria x)
                                    | x == 'A'  = (Maestria x)
                                    | y == 'A' = (Maestria y)
-}
ordenarBurbuja lista = ordenarBurbujaRec lista (length lista)

ordenarBurbujaRec lista 0 = lista
ordenarBurbujaRec lista n = ordenarBurbujaRec (iterarBurbuja lista) (n - 1)

iterarBurbuja [] = []
iterarBurbuja [x] = [x]
iterarBurbuja (x:y:resto)
    | x > y     = y : iterarBurbuja (x:resto)
    | otherwise = x : iterarBurbuja (y:resto)

instance Eq Nota where 
    (Umss n1) == (Umss n2) = n1 == n2






estaOrdenadaNota :: [Nota] -> Bool
estaOrdenadaNota [x] = True
estaOrdenadaNota ((Umss _):(Umss n):xs)  = estaOrdenadaNota ((Umss n):xs)
estaOrdenadaNota ((Cato _ _):(Cato n1 n2):xs)  = estaOrdenadaNota ((Cato n1 n2):xs)
estaOrdenadaNota ((Maestria _):(Maestria n):xs)  = estaOrdenadaNota ((Maestria n):xs)
estaOrdenadaNota ((Maestria _):_:xs)  = False
estaOrdenadaNota (_:(Maestria n):xs)  = estaOrdenadaNota ((Maestria n):xs)
estaOrdenadaNota ((Umss _):(Cato _ _):xs)  = False
estaOrdenadaNota ((Cato _ _):(Umss n):xs)  = estaOrdenadaNota ((Umss n):xs)


{-5. Inventar 3 tipos de datos compuestos, para cada tipo inventar 3 funciones que se
apliquen al mismo

6. Definir una función que reciba 4 número y devuelva la suma del mayor de los 2
primeros con el mayor de los 2 siguientes utilizar la función mayor definida
data Rpta = Entero Int| Mensaje String
mayor x y | x > y = Entero x
| y > x = Entero y
| otherwise = Mensaje “Iguales”

7. Definir un tipo de datos para representar números enteros (Positivos y negativos).
Utilizando este tipo definir los operadores: +,-,*,div

8. Definir una función que reciba dos listas xs, ys (del tipo Lista a) y devuelva cuantas
veces ocurre xs en ys.-}


data Nota2 = Numero Int | Letra Char
    deriving Show

queNota (Numero n) = (Numero n)
queNota (Letra n) = (Letra n)


data Rpta = Entero Int| Mensaje String
    deriving Show
mayor23 x y | x > y = Entero x
          | y > x = Entero y
          | otherwise = Mensaje "Iguales"

sonIguales x y = case mayor23 x y of
    Mensaje "Iguales" -> Entero x
    resultado -> resultado

sumaM (Entero x) (Entero y) = Entero (x+y)

sumaMayores x1 x2 y1 y2 =sumaM (sonIguales x1 x2 ) (sonIguales y1 y2)

instance Eq Rpta where 
    (Mensaje x) == (Mensaje y) = x==y





data Positivos = Positivo Int | Negativo Int
    deriving Show 


miltiplicacionData (Positivo x) (Positivo y) = Positivo (x*y)
miltiplicacionData (Negativo x) (Negativo y) = Negativo (x*y)
miltiplicacionData (Negativo x) (Positivo y) = Negativo (x*y)
miltiplicacionData (Positivo x) (Negativo y) = Negativo (x*y)

sumaData (Positivo x) (Positivo y) = Positivo (x+y)
sumaData (Negativo x) (Negativo y) = Negativo (x+y)
sumaData (Negativo x) (Positivo y) = if x>=y then Negativo (x-y) else Positivo (x-y)
sumaData (Positivo x) (Negativo y) = if x>=y then Positivo (x-y) else Negativo (x-y)


data Tree a = Nada | Rami a (Tree a) (Tree a)
    deriving Show


prof Nada = 0
prof (Rami x ai ad) = 1 +  max(prof ai)(prof ad) 

peso Nada = 0
peso (Rami x ai ad) = (prof ad)-(prof ai)

a1T = Rami 50 (Rami 40 Nada Nada)(Rami 60 Nada Nada)

a2T = Rami 200 (Rami 150 Nada Nada)(Rami 250 Nada Nada)

a3T = Rami 100 (a1T) (a2T)

balancear a@(Rami x ai ad)
            |peso a == -2 && peso ai == -1 = caso1 a
            |peso a == 2 && peso ad == 1 = caso2 a
            |peso a == -2 && peso ai == 1 = caso3 a
            |peso a == 2 && peso ad == -1 = caso4 a 
            |otherwise  = a


        
caso1 (Rami y (Rami z ai2 ad2) ad1) = Rami z ai2 (Rami y ad2 ad1)
caso2 (Rami y ai1 (Rami z ai2 ad2)) = Rami z (Rami y ai2 ai1) ad2
caso3 (Rami y ai (Rami z ad1 ad2)) = Rami z (Rami y ai ad1) ad2
caso4 (Rami y (Rami z ai1 ai2) ad) = Rami z ai1 (Rami y ai2 ad)


arbolP1 = Rami 20 (Rami 10 (Rami 5 Nada Nada) Nada)(Rami 30 Nada Nada)
arbolP2 = Rami 40 (arbolP1)(Rami 60 Nada Nada) 
arbolP3 = Rami 120 (Rami 110 Nada Nada) (Rami 130 Nada Nada)
arbolP4 = Rami 180 (Rami 170 Nada Nada) Nada
arbolP5 = Rami 150 (arbolP3) (arbolP4) 
arbolPFin = Rami 80 (arbolP2) (arbolP5)

pCaso2 = Rami 100 Nada (pCaso22)
pCaso22 :: Tree Integer
pCaso22 = Rami 150 Nada (Rami 152 Nada Nada) 
                      

data ArbolDe3 a b = Hojita a | Ramita b (ArbolDe3 a b) (ArbolDe3 a b) (ArbolDe3 a b)
        deriving Show  



arb1 = Ramita 'a' (Hojita 1) (Hojita 2) (Hojita 3)
arb2 = Ramita 'b' (arb1) (arb1) (arb1)



arbolTupla arb = (sumatoriaArb arb , listaArb arb)

sumatoriaArb (Hojita x) = x
sumatoriaArb (Ramita _ ai am ad) = (sumatoriaArb ai) + (sumatoriaArb am) + (sumatoriaArb ad)

listaArb (Hojita _ ) = ""
listaArb (Ramita x ai am ad) = x : ((listaArb ai) ++ (listaArb am) ++ (listaArb ad))  
