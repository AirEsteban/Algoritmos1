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
paratodo :: [Bool] -> Bool
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
pertenece _ [] = False
pertenece n (x:xs) = (x==n) || pertenece n xs

--Ejercicio 4
encuentra :: Int ->[(Int, String)] -> String
encuentra _ [] = ""
encuentra n ((x,y):xs) | (n==x) = y
                       | otherwise = encuentra n xs

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
paratodo2 :: [a] -> (a -> Bool) -> Bool
paratodo2 (x:xs) funcion = paratodo' (x:xs) funcion
paratodo2 [] _ = False -- por warning de non exhaustive pattern

--Ejercicio 7
--a)
esPar :: Int -> Bool
esPar n = mod n 2 == 0
todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs esPar 

--b)
esMultiplo :: Int -> Int -> Bool
esMultiplo n m = (mod n m == 0)
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo i xs = existe' xs (esMultiplo i)

--c)
sumaCuadrados :: Int -> Int
sumaCuadrados x | (x>=0) = sumatoria' [0..(x-1)] (\y->y*y) -- lambda funcion que utiliza su propio scope
                | otherwise = error "El numero ingresado no puede ser negativo"

--d)
factorial' :: Int -> Int
factorial' x = productoria' [1..x] (+0)

--e)
tomarPares :: [Int] -> [Int]
tomarPares [] = []
tomarPares (x:xs) | (mod x 2) == 0 = x : tomarPares xs
                  | otherwise = tomarPares xs
multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria (tomarPares xs)


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
duplica' :: [Int] -> [Int]
duplica' xs = map (2*) xs

--Ejercicio 10
--a) 
buscaPares :: [Int] -> [Int]
buscaPares [] = []
buscaPares (x:xs) | mod x 2 == 0 = x : buscaPares xs
                  | otherwise = buscaPares xs
--b)
buscaPares' :: [Int] -> [Int]
buscaPares' xs = filter (esPar) xs

--c)
multiplicaPares' :: [Int] -> Int
multiplicaPares' xs = productoria (filter (esPar) xs)

--Ejercicio 11
--a)
-- 1
sumarALista :: Num a => a -> [a] -> [a]
sumarALista _ [] = []
sumarALista n (x:xs) = (x+n) : sumarALista n xs

-- 2
encabezar :: a -> [[a]] -> [[a]]
encabezar _ [] = []
encabezar e [[]] = [[e]]
encabezar e (xs:ys) = ((e:xs):(encabezar e ys))

-- 3
mayoresA :: Ord a => a -> [a] -> [a]
mayoresA _ [] = []
mayoresA n (x:xs) | (x > n) = x: mayoresA n xs
                  | otherwise = mayoresA n xs

--b)
-- 1
sumarALista' :: Num a => a-> [a] -> [a]
sumarALista' n xs = map (+n) xs

--2
agregar :: a -> [a] -> [a]
agregar n [] = [n]
agregar n xs = n : xs
encabezar' :: a -> [[a]] -> [[a]]
encabezar' n ys = map (agregar n) ys

--3
mayoresA' :: Ord a => a-> [a] -> [a]
mayoresA' n xs = filter (>n) xs

--Ejercicio 12

igualInt :: Int -> (Int, String) -> Bool
igualInt x (a,_) = (x==a)

sndHead :: [(Int, String)] -> String
sndHead xs | xs == [] = ""
           | otherwise = snd (head xs)

encuentra' :: Int -> [(Int,String)] -> String
encuentra' _ [] = ""
encuentra' x xs = sndHead (filter (igualInt x ) xs)

encuentra'' :: Int -> [(Int,String)] -> String
encuentra'' x = sndHead . filter (igualInt x )

--filter (igualInt x) :: [(Int,String)] -> [(Int,String)] 
--sndHead :: [(Int, String)] -> String
--sndHead . filter (igualInt x ) :: [(Int, String)] -> String

comp :: (b -> c) -> (a -> b) -> (a -> c)
comp g f a = g (f a)
--Ejercicio 13
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

--a)
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x : (y:xs)) | (x==y) = x : primIguales (y:xs)  
                         | otherwise = [x]
--b)
primIguales' :: Eq a => [a] -> [a]
primIguales' [] = []
primIguales' (x:xs) = primIgualesA' x (x:xs)

--Ejercicio 15
minimo :: (Bounded a, Ord a) => [a] -> a
--b)
minimo [] = minBound :: []
--a)
minimo [x] = x
minimo (x:y:xs) | (x < y)  =  minimo (x:xs)
                | (x >= y)  = minimo (y:xs)
minimo (_:_:_) = error "comparacion erronea"


--Ejercicio 16
{--
a)f :: (a, b) -> ...
f x = 

Respuesta: Está bien tipado, pues x puede ser una tupla y si cumple todos los casos de la definición.

b)
f :: (a, b) -> ...
f (x , y) = ...

Respuesta: Si está bien tipado, pues la variable 'x' es de tipo a y la variable 'y' es de tipo b

c)
f :: [(a, b)] -> ...
f (a , b) = ...

Respuesta: No se encuentra bien tipado, el parámetro que se le asigna es una tupla, mientras que la función requiere una lista de tuplas.

d)
f :: [(a, b)] -> ...
f (x:xs) = ...

Respuesta: Si bien 'x' puede hacer referencia a tuplas, esto no me asegura que x sea realmente una tupla, por lo tanto x puede tomar cualquier otro tipo de variable y esto podría fallar por ejemplo siendo x un entero, a su vez, no tiene en cuenta el caso de listas vacías.

e)
f :: [(a, b)] -> ...
f ((x, y) : ((a, b) : xs)) = ...

Respuesta: Bien tipado, pues se pasa con pattern-matching una lista de tuplas, pero no cubre el caso de listas vacías ni de un solo elemento, ahí solo es para listas de dos o más tuplas.

f)
f :: [(Int, a)] -> ...
f [(0, a)] = ...

Respuesta: Bien tipado, pero no cubre el caso de listas vacías.

g)
f :: [(Int, a)] -> ...
f ((x, 1) : xs) = ...

Respuesta: Dependiendo del valor que tome x está bien tipado, pero no cubre el caso de listas vacías, además en la parte de 1, para "ayudar" a especificar debería agregarse 1::Int

h)
f :: [(Int, a)] -> ...
f ((1, x) : xs) = ...

Respuesta: Bien tipado, pero no cubre el caso de listas vacías.

i)
f :: (Int -> Int) -> Int -> ...
f a b = ...

Respuesta: Bien tipado pues 'a' puede ser una función y b un entero, aunque si b no es entero, explota.

j)
f :: (Int -> Int) -> Int -> ...
f a 3 = ...

Respuesta: Bien tipado si 'a' es una función que va de Int -> Int, y 3 es un entero.

k)
f :: (Int -> Int) -> Int -> ...
f 0 1 2 = ...

Respuesta: Mal tipado, pues  se le están pasando más parámetros de los necesarios para la función. 

l)
f :: a -> (a -> a) -> ...
f a g = ...

Respuesta: Bien tipado, pues a puede ser cualquier tipo y g puede ser una función de tipo (a -> a)

--} 

-- Ejercicio 17

{--
a)
f :: (a, b) -> a
Respuesta: No es posible otra definición además de la listada abajo
f (x,_) = x

b)
f :: (a, b) -> b
Respuesta: No es posible otra definición además de la listada abajo
f (_,b) = b

c)
f :: (a, b) -> c
Respuesta: No es posible sin especificar los tipos, ya que no hay "operadores generales" que funcionen con variables de tipo general.

d)
f :: a -> b
Respuesta: No puedo realizar una conversión de tipos generales, ahora lo que se podría hacer por ejemplo es realizar una división de números enteros que me asegure que me da un número con coma, y estaría cambiando de tipo... por ejemplo siendo a de tipo Integer a / (a+1) pero nuevamente, al tener tipos generales, no puedo utilizar estos operadores.

e)
f :: (a -> b) -> a -> b
Respuesta: f funcionAux x = funcionAux x

f)
f :: (a -> b) -> [a] -> [b]

g)
f :: (a -> b) -> a -> c
Respuesta: No puedo dar una definición que me cambie o mezcle los tipos, siendo éstos generales.


h)
f :: (a -> b) -> (b -> c) -> a -> c
Respuesta: comp :: (b -> c) -> (a -> b) -> (a -> c)
comp g f a = g (f a)


--}


































