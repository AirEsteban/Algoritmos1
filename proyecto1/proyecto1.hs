-- ejercicio 1

--A
esCero :: Int -> Bool
esCero n = (n==0)

--B
esPositivo :: Int -> Bool
esPositivo n = (n>0)

-- C
esVocal :: Char -> Bool
esVocal n = (n=='a') || (n=='e') || (n=='i') || (n=='o') || (n== 'u')

-- Ejercicio 2

--a).
paratodo:: [Bool] -> Bool
paratodo []= True
paratodo (x:xs) = (x==True) && paratodo xs

--b)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs 

--c)
productoria :: [Int] -> Int
productoria[] = 1
productoria (x:xs) = x * productoria xs

--d)
factorial :: Int -> Int
factorial 0 = 1
factorial n =  n * factorial (n-1) 

--e)
promedio :: [Int] -> Int
promedio [] = 0
promedio xs = div (sumatoria xs) (length xs)

--Ejercicio 3 programa la funcion pertenece :: Int -> [Int] -> Bool, que verifica si un numero se encuentra en una lista
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = (x==n) || pertenece n xs

--Ejercicio 4
encuentra :: Int ->[(Int, String)] -> String
encuentra n [] = ""
encuentra n ((x,y):xs) | (n==x) = y
		       | (n/=x) = encuentra n xs

--Ejercicio 5
--Program´a las siguientes funciones que implementan los cuantificadores generales. Nota que el segundo parametro de cada funcion, es otra funcion! a) paratodo’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el predicado t.

paratodo' :: [Int] -> (Int -> Bool) -> Bool
paratodo' ()
 
--b) existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si alg´un elemento de xs satisface el predicado t.

--c) sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una funci´on t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la suma de los valores que resultan de la aplicaci´on de t a los elementos de xs.

--d) productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a] y una funci´on t :: a -> Int, calcula el producto de los valores que resultan de la aplicaci´on de ta los elementos de xs.
























