
estaEnLista [] _ = True
estaEnLista _ [] = False
estaEnLista (x:xs) (y:ys) | x ==  y = estaEnLista xs ys 
                          |otherwise = estaEnLista (x:xs) ys

hayEnlista [] _ = True
hayEnlista _ [] = False
hayEnlista (x:xs) (y:ys) | x==y = hayEnlista xs ys  
                         |otherwise = False

posiciones xs [] i = []
posiciones xs (y:ys) i = if hayEnlista xs (y:ys) == True then i : posiciones xs ys (i+1) else posiciones xs ys (i+1)
sobrecargaPosiciones xs ys = posiciones xs ys 1


data Lista a = Vacio | Add a (Lista a)
    deriving Show


instance Eq a => Eq (Lista a) where 
    Vacio == Vacio = True
    (Add x xs) == (Add y ys) = x == y && xs == ys

listaEsta Vacio _ = True
listaEsta _ Vacio = False
listaEsta (Add x xs) (Add y ys) |x == y = listaEsta xs ys
                                |otherwise = False


hayENLista _ Vacio  i = Vacio
hayENLista xs (Add y ys) i =   if listaEsta xs (Add y ys) then Add i ((hayENLista xs ys) (i+1)) else hayENLista xs ys (i+1)

sobrecargaHayenLista xs ys = hayENLista xs ys 1

l1 = (Add 1 (Add 2 Vacio))
l2 = (Add 3 (Add 1 (Add 2 (Add 2 (Add 1 (Add 2 Vacio ))))))

-- define la funcion map en foldr
mapFoldr f xs = foldr (\x xs -> f x : xs) [] xs