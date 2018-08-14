-- Ejercicio 1
--a)
esCero :: Int -> Bool
esCero n = (n==0)

--b)
esPositivo :: Int -> Bool
esPositivo n = (n>0)

-- c)
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

--Ejercicio 3
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = (x==n) || pertenece n xs

--Ejercicio 4
encuentra :: Int ->[(Int, String)] -> String
encuentra n [] = ""
encuentra n ((x,y):xs) | (n==x) = y
		       | (n/=x) = encuentra n xs

--Ejercicio 5
--Programa las siguientes funciones que implementan los cuantificadores generales. Nota que el segundo parametro de cada funcion, es otra funcion! a) paratodo’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el predicado t.

--paratodo' :: [Int] -> (Int -> Bool) -> Bool
--paratodo' [] esPositivo = False
--paratodo’ (x:xs) esPositivo 
 
--b) existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si alg´un elemento de xs satisface el predicado t.

--c) sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una funci´on t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la suma de los valores que resultan de la aplicaci´on de t a los elementos de xs.

--d) productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a] y una funci´on t :: a -> Int, calcula el producto de los valores que resultan de la aplicaci´on de ta los elementos de xs.

--Ejercicio 6

--Ejercicio 7

--Ejercicio 8
	--filter :: (a -> Bool) -> [a] -> [a] Source#
	--filter, applied to a predicate and a list, returns the list of those elements that satisfy the predicate; i.e.,
	--filter p xs = [ x | x <- xs, p x]
	--map :: (a -> b) -> [a] -> [b] Source#
	--map f xs is the list obtained by applying f to each element of xs, i.e.,
	--map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
	--map f [x1, x2, ...] == [f x1, f x2, ...]
		--¿A qué equivale la expresión map succ [1, -4, 6, 2, -8], donde succ n = n+1?
			-- succ [1,-4,6,2,-8] == [2,-3,7,3,-9] (Reemplaza cada valor por su sucesor)
		--¿Y la expresión filter esPositivo [1, -4, 6, 2, -8]?
			-- esPositivo [1, -4, 6, 2, -8] == [1,6,2] (Filtra los valores negativos)

--Ejercicio 9

--Ejercicio 10

--Ejercicio 11

--Ejercicio 12

--Ejercicio 13

--Ejercicio 14

--Ejercicio 15

--Ejercicio 16

--Ejercicio 17







