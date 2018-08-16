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

--Ejercicio 5
--a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] funcion = True
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

--Ejercicio 7
--a)
esPar :: Int -> Bool
esPar n = mod n 2 == 0
todosPares :: [Int] -> Bool
todosPares (x:xs) = paratodo' (x:xs) esPar 

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
sumaCuadrados n = sumatoria'' [0..n] 

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



























