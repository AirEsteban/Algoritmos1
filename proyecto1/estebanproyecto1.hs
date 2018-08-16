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
pertenece n [] = False
pertenece n (x:xs) = (x==n) || pertenece n xs

--Ejercicio 4
encuentra :: Int ->[(Int, String)] -> String
encuentra n [] = ""
encuentra n ((x,y):xs) | (n==x) = y
		       | (n/=x) = encuentra n xs

--Ejercicio 5
--a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] funcion = True
paratodo' (x:xs) funcion = funcion x && paratodo' xs funcion
 
--b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] funcion = False
existe' (x:xs) funcion = funcion x || existe' xs funcion

--c)
masuno n = n + 1
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] funcion = 0
sumatoria' (x:xs) funcion = funcion x + (sumatoria' xs funcion)

--d)
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] funcion = 1
productoria' (x:xs) funcion = funcion x * productoria' xs funcion

--Ejercicio 6
paratodo :: [a] -> (a -> Bool) -> Bool
paratodo (x:xs) funcion = paratodo' (x:xs) funcion

--Ejercicio 7
--a)
esPar :: Int -> Bool
esPar n = mod n 2 == 0
todosPares (x:xs) = paratodo' (x:xs) esPar 

--b)
esMultiplo :: Int -> Int -> Bool
esMultiplo n m = (mod n m) == 0
existe'' n [] funcion = False
existe'' n (x:xs) funcion = funcion n x || existe'' n xs funcion
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo i (x:xs) = existe'' i (x:xs) esMultiplo 

--c)
sumatoria'' :: [Int] -> Int -- ver lo de hacer con [a] -> Int, haciendo que solamente funcione con numeros poniendo Num [a]
sumatoria'' [] = 0
sumatoria'' (x:xs) = (x*x) + (sumatoria'' xs)
sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria'' [0..n] 

--d)¿Se te ocurre como redefinir factorial (ej. 2d) para evitar usar recursi ́on?
productoria'' [Int] -> Int -- ver lo de hacer con [a] -> Int,
factorial' :: Int -> Int 

factorial' = productoria'


--e)multiplicaPares :: [Int] -> Int que calcula el producto de todos los n ́umeros pares de una lista.


--Ejercicio 8

























