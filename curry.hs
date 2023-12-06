import System.Win32 (COORD(yPos), xBUTTON1)


f13 :: Int -> Int -> Int -> Int
f13 x y z = x*2+y

f16 :: (a -> b -> c) -> a -> b -> c
f16 x y z = x y z


f17 :: Bool -> Bool -> Bool -> Bool
f17 x y z | x = y
          | y = z


f8 :: (y -> x) -> (z->y) -> z -> x
f8 x y z = x (y z)

f10 :: (y -> x) -> (z->y) -> (w->z) -> w -> x
f10 x y z w= x (y (z w))



f9 :: (x->y->z) -> x -> y ->z
f9 x y z = (x y) z



mescla xs [] = xs 
mescla [] ys = ys
mescla (x:xs) (y:ys) = x : (y : (mescla xs ys))


takee n _ | n <= 0 = []
takee i (x:xs) = if i == 0 then [] else x : takee (i-1) xs
rampa [] i = []
rampa (x:xss) i = takee i x : rampa xss (i+1)

rampaa xss = rampa xss 1    

data Arbol a = Hoja a | Rama a (Arbol a) (Arbol a) deriving Show


f3 (Hoja x) = x
f3 (Rama x ai ad) = x + f3 ai + f3 ad


instance (Eq a) => Eq (Arbol a) where 
    (Hoja x) == (Hoja y) = x== y
    (Rama x ai1 ad1) == (Rama y ai2 ad2 ) = x == y && ai1 == ai2 && ad1 == ad2


data Lista a = Vacia | Add a (Lista a) 
    deriving Show

instance (Eq a) => Eq (Lista a) where 
    Vacia == Vacia  = True 
    (Add x xs) == (Add y ys) = x == y && xs == ys
    _ == _ = False

ese Vacia = 0
ese (Add x xs) = x + ese xs

l1 = [1,2,3,4,5,6]
l2 = [5,4,3,2,1,6]



varia [] [] n = n
varia (x:xs) (y:ys) n | x==y = varia xs ys n 
                      |otherwise = varia xs ys (n+1)

final xs ys | varia xs ys 0 > 2 = False 
            |otherwise = True


tamano xs ys | length xs == length ys = True 
             |otherwise = False


total xs ys |tamano xs ys == False = False
            |final xs ys == False = False
            |otherwise = True

type Dia = Int
type Mes= Int
type Anio = Int
type Fecha=(Dia,Mes,Anio)


juan :: Presi
juan = ("juan",1900)


sacarDia :: Fecha -> Dia
sacarDia(x,_,_) = x


type Presi = (Nomnre, Ano)
type Nomnre = String
type Ano = Int


sacarN (n,_) = n


terce xs = head (drop 2 xs)

