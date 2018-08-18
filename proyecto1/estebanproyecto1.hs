-- Ejercicio 1

--a)
esCero :: Int -> Bool
esCero n = (n==0)

--b)
esPositivo :: Int -> Bool
esPositivo n = (n>0)

--c)
esVocal :: Char -> Bool
esVocal n = (n=='a') || (n=='e') || (n=='i') || (n=='o') || (n== 'u')

-- Ejercicio 2

--a)
--paratodo :: [Bool] -> Bool -- comentado para el ejercicio 6
--paratodo []= True
--paratodo (x:xs) = (x==True) && paratodo xs

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
pertenece _ [] = False
pertenece n (x:xs) = (x==n) || pertenece n xs

--Ejercicio 4
encuentra :: Int ->[(Int, String)] -> String
encuentra _ [] = ""
encuentra n ((x,y):xs) | (n==x) = y
                       | (n/=x) = encuentra n xs
encuentra _ ((_, _) : _) = "" -- por warning de non exhaustive pattern

--Ejercicio 5
--a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True
paratodo' (x:xs) funcion = funcion x && paratodo' xs funcion
 
--b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] _ = False
existe' (x:xs) funcion = funcion x || existe' xs funcion

--c)
masuno :: Int -> Int
masuno n = n + 1
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] _ = 0
sumatoria' (x:xs) funcion = funcion x + (sumatoria' xs funcion)

--d)
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) funcion = funcion x * productoria' xs funcion

--Ejercicio 6
paratodo :: [a] -> (a -> Bool) -> Bool
paratodo (x:xs) funcion = paratodo' (x:xs) funcion
paratodo [] _ = False -- por warning de non exhaustive pattern

--Ejercicio 7
--a)
esPar :: Int -> Bool
esPar n = mod n 2 == 0
todosPares :: [Int] -> Bool
todosPares (x:xs) = paratodo' (x:xs) esPar 
todosPares [] = False -- por warning de non exhaustive pattern

--b)
esMultiplo :: Int -> Int -> Bool
esMultiplo n m = (mod n m == 0)
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo i xs = existe' xs (esMultiplo i)

--c)
sumatoria'' :: [Int] -> Int -- ver lo de hacer con [a] -> Int, haciendo que solamente funcione con numeros poniendo Num [a]
sumatoria'' [] = 0
sumatoria'' (x:xs) = (x*x) + (sumatoria'' xs)
sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria'' [0..n-1] 

--d)
productoria'' :: [Int] -> Int -- ver lo de hacer con [a] -> Int,
productoria'' [] = 1
productoria'' (x:xs) = x * productoria xs
factorial' :: Int -> Int 
factorial' n = productoria'' [1..n]

--e)multiplicaPares :: [Int] -> Int que calcula el producto de todos los n ́umeros pares de una lista.
tomarPares :: [Int] -> [Int]
tomarPares [] = []
tomarPares (x:xs) | (mod x 2) == 0 = x : tomarPares xs
                  | (mod x 2) /= 0 = tomarPares xs
tomarPares (_:_) = [] -- por warning de non exhaustive pattern
multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria (tomarPares xs)

-- PREGUNTAR SOBRE LOS WARNINGS DE NON-EXHAUSTIVE

--Ejercicio 8
--a)
-- La función map f xs lo que hace es aplicar f a cada elemento de xs y devuelve una lista del mismo tamaño que xs.

-- La función filter p xs  lo que hace es devolver una lista con los elementos que satisfacen el predicado p.

--b)
-- La expresión map succ [1, -4, 6, 2, -8] donde succ n = n+1 equivale a [2,-3,7,3,-7]

--c)
-- La función filter esPositivo [1, -4, 6, 2, -8] equivale a [1,6,2]

--Ejercicio 9
--a)
duplica :: [Int] -> [Int]
duplica [] = []
duplica [x] = [2*x]
duplica (x:xs) = 2 * x : duplica xs

--b)
doble :: Int->Int
doble x = 2 * x
duplica' :: [Int] -> [Int]
duplica' xs = map doble xs

--Ejercicio 10
--a) 
buscaPares :: [Int] -> [Int]
buscaPares [] = []
buscaPares (x:xs) | mod x 2 == 0 = x : buscaPares xs
                  | mod x 2 /= 0 = buscaPares xs
buscaPares (_:_) = [] -- por warning de non exhaustive pattern

--b)
buscaPares' :: [Int] -> [Int]
buscaPares' xs = filter (esPar) xs

--c)
-- Utilizando el filter para encontrar los pares y eso pasaría a ser parámetro de la función productoria para así retornar el producto de todos los números pares.
multiplicaPares' :: [Int] -> Int
multiplicaPares' xs = productoria (filter (esPar) xs)

--Ejercicio 11
--a)
-- 1
sumarALista :: Num a => a -> [a] -> [a]
sumarALista _ [] = []
sumarALista n (x:xs) = (x+n) : sumarALista n xs

-- 2 que toma un valor de tipo a y lo introduce en la cabeza de cada lista del segundo par ́ametro 
{--encabezar:: a -> [[a]] -> [[a]]
encabezar _ [] = []
encabezar n ((x:xs):ys) = (n : (x : xs)) : encabezar n ys
encabezar _ ([] : _) = [] --} -- ver que hacer con lo del vacío 

-- 3 -- ver warning que pasa
mayoresA :: Ord a => a -> [a] -> [a]
mayoresA _ [] = []
mayoresA n (x:xs) | (x > n) = x: mayoresA n xs
                  | otherwise = mayoresA n xs

--b)
-- 1
sumarALista' :: Num a => a-> [a] -> [a]
sumarALista' n xs = map (+n) xs -- ver warning defaulting consstraints to type 'Integer'

--2
{--encabezar' :: a -> [[a]] -> [[a]]
encabezar' n ((_:_):ys) = map (n:) ys--} -- ver como hace lo del vacio tambien, o para listas unitarias

--3
mayoresA' :: Ord a => a-> [a] -> [a]
mayoresA' n xs = filter (>n) xs   -- ver warning defaulting consstraints to type 'Integer'

--Ejercicio 12

{--encuentra' :: Int ->[(Int, String)] -> [(Int, String)]
encuentra' n ((x,y):ys) = filter (x==n) y --}

--Ejercicio 13 La funci ́on primIgualesA toma un valor y una lista, y calcula el tramo inicial m ́as largo de la lista cuyos elementos son iguales a ese valor
--a)
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA n (x:xs) | x==n = x : primIgualesA n xs
                      | x/=n = primIgualesA n xs
primIgualesA _ (_ : _) = error "No es posible" -- ver warning defaulting

--b)
primIgualesA' :: Eq a=> a-> [a] -> [a]
primIgualesA' n xs = takeWhile (==n) xs

--Ejercicio 14







































