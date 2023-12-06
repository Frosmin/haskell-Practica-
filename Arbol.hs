data Arbolde3 a = Hoja a | Rama (Arbolde3 a) (Arbolde3 a) (Arbolde3 a)
    deriving Show



instance Eq a => Eq (Arbolde3 a) where
    Hoja x == Hoja y = x == y 
    Rama ai am ad == Rama ai2 am2 ad2 = ai == ai2 && am == am2 && ad == ad2
    _ == _  = False 

ejemploArbol =
  Rama
    (Hoja 1)
    (Rama
      (Hoja 2)
      (Hoja 3)
      (Hoja 4)
    )
    (Rama
      (Rama (Hoja 5) (Hoja 6) (Rama (Hoja 1)(Hoja 2)(Hoja 3)))
      (Hoja 8)
      (Hoja 10)
    )

altura (Hoja _) = 1
altura (Rama ai ad am) = 1 +  max (max (altura ai) (altura am)) (altura ad) 


mapArbol f (Hoja x) = (Hoja (f x))
mapArbol f (Rama ai am ad) = Rama (mapArbol f ai) (mapArbol f am) (mapArbol f ad)

concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys