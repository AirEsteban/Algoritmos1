--Ejercicio 1 
--a) 
data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado deriving (Show,Eq)

--b)
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matemática"
titulo Fisica = "Licenciatura en Física"
titulo Computacion = "Licenciatura en ciencias de la computacion"
titulo Astronomia = "Licenciado en Astronomia"
titulo Profesorado = "Profesorado"

--c) No se puede realizar un análisis por casos pues no puedo operar con tipos sin tener previamente definido algun operador.

--Ejercicio 2
--a)
type Ingreso = Int

data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Show,Eq)
data Area = Administrativa | Ensenanza | Economia | Postgrado deriving (Show,Eq)

data Rol = Decano 
           | Docente Cargo
           | NoDocente Area
           | Estudiante Carrera Ingreso
           deriving (Show,Eq)

--b) El tipo de constructor de Docente es del nuevo tipo creado anteriormente Cargo

--c) y d)
verCargo :: Rol -> Cargo
verCargo (Docente c) = c --Para obtener el cargo del docente y usarlo en cuantosDoc
verCargo _ = error "" -- ver esto.

esDocente :: Rol -> Bool
esDocente (Docente _) = True
esDocente _= False -- Para el filter, ver si es docente o no

soloDoc :: [Rol] -> [Rol]
soloDoc [] = []
soloDoc xs = filter (esDocente) xs -- De una lista roles, me devuelve solo los docentes

contDoc :: [Rol] -> Cargo -> Int
contDoc [] _ = 0
contDoc (x:xs) Titular = case verCargo x of
                             Titular -> 1 + cuantos_doc xs Titular
                             _ -> 0 + cuantos_doc xs Titular
contDoc (x:xs) Asociado = case verCargo x of
                             Asociado -> 1 + cuantos_doc xs Asociado
                             _ -> 0 + cuantos_doc xs Asociado
contDoc (x:xs) Adjunto = case verCargo x of
                             Adjunto -> 1 + cuantos_doc xs Adjunto
                             _ -> 0 + cuantos_doc xs Adjunto
contDoc (x:xs) Asistente = case verCargo x of
                             Asistente -> 1 + cuantos_doc xs Asistente
                             _ -> 0 + cuantos_doc xs Asistente
contDoc (x:xs) Auxiliar = case verCargo x of
                             Auxiliar -> 1 + cuantos_doc xs Auxiliar
                             _ -> 0 + cuantos_doc xs Auxiliar

cuantos_doc :: [Rol] -> Cargo -> Int
cuantos_doc xs c = contDoc (soloDoc xs) c 

pruebaCuantosDoc :: [Rol]
pruebaCuantosDoc = [Decano,Docente Adjunto, Docente Adjunto, Docente Auxiliar, Docente Asistente, Decano, Estudiante Computacion 2018, Decano, Docente Asistente, Docente Asociado,Docente Asociado, Docente Asociado]

--e) Modificando el constructor de Decano, asignandole un parámetro de que lo represente. Por ej el parámetro Bool,
-- siendo True para la mujer y False para el hombre (ésto previamente acordado) sino creando otro Nuevo tipo, llamado Genero con sus correspondientes constructores Hombre y Mujer.

--f) Para representar un alumno que está inscripto en dos carreras podríamos hacer un sinónimo de 
-- tipo en el que la parte del constructor de carrera se pase una tupla con las dos carreras o análogamente, una lista
-- por si se quiere que tenga más de dos carreras.

getCarrera :: Carrera -> Carrera
getCarrera Matematica = Matematica
getCarrera Fisica = Fisica
getCarrera Computacion = Computacion
getCarrera Astronomia = Astronomia
getCarrera Profesorado = Profesorado

estudia :: Rol -> Carrera -> Bool
estudia (Estudiante c _) car | getCarrera c == car = True
                             | otherwise = False 
estudia _ _ = error "Sólo los estudiantes pueden cursar carreras."  -- ver como manejar errores...

-- Ejercicio 3
--a)
data Persona = Per String String Int Int Int Int Rol deriving (Eq,Show)
--data Per1 = Per1 String String Int Int Int Int Rol

--b)No ya que Per es un constructor del Tipo Persona.

--c) 
--1 
edad :: Persona -> (Int,Int,Int) -> Int
edad (Per _ _ _ dia mes ano _) (d,m,a) = abs(div ((d-dia) + ((m-mes) * 30) + ((a-ano) * 365)) 365)

--2
existe :: String -> [Persona] -> Bool
existe _ [] = False
existe apellido (Per ape _ _ _ _ _ _ : xs) = apellido == ape || existe apellido xs
--(Prueba) existe "airasca" [Per "jeje" "petrone" 23 14 13 2012 (NoDocente Administrativa),Per "airasca" "esteban" 23 14 13 2012 (NoDocente Administrativa),Per "bon jovi" "jon" 23 14 13 2012 (NoDocente Administrativa) ,Per "quito" "esteban" 23 14 13 2012 (NoDocente Administrativa)]

--3
--(Prueba) est_astronomia [Per "jeje" "petrone" 23 14 13 2012 (Estudiante Astronomia 2017),Per "airasca" "esteban" 23 14 13 2012 (Estudiante Matematica 2015),Per "bon jovi" "jon" 23 14 13 2012 (NoDocente Administrativa) ,Per "quito" "esteban" 23 14 13 2012 (Estudiante Astronomia 2017),Per "julio" "juli" 23 14 13 2012 Decano]

soloAstro :: Persona -> Bool
soloAstro (Per _ _ _ _ _ _ (Estudiante Astronomia _)) = True
soloAstro _ = False 

est_astronomia :: [Persona] -> [Persona]
est_astronomia [] = []
est_astronomia xs = filter (soloAstro) xs

--4
getDatosNoDocente :: Persona -> (String,Int)
getDatosNoDocente (Per ape nom doc _ _ _ (NoDocente _)) = (ape ++ ", " ++ nom,doc)
getDatosNoDocente (Per _ _ _ _ _ _ _) = ("",0)
 
nodocente :: Persona -> Bool
nodocente (Per _ _ _ _ _ _ (NoDocente _)) = True
nodocente _ = False

padron_nodocente :: [Persona] -> [(String, Int)]
padron_nodocente [] = []
padron_nodocente xs = map (getDatosNoDocente) (filter (nodocente) xs)

--Ejercicio 4
data Cola = Vacia | Encolada Persona Cola deriving (Show,Eq)
--a)
--1
--(Prueba) atender (Encolada (Per "jeje" "petrone" 23 14 13 2012 (Estudiante Astronomia 2017)) (Encolada (Per "jej" "asd" 23 14 13 2012 (Estudiante Astronomia 2017)) Vacia))
atender :: Cola -> Cola
atender Vacia = Vacia
atender (Encolada _ cola) = cola

--2
--(Prueba)  encolar (Per "jeje" "petrone" 23 14 13 2012 (Estudiante Astronomia 2017)) Vacia
--(Prueba)  encolar (Per "jeje" "petrone" 23 14 13 2012 (Estudiante Astronomia 2017)) (Encolada (Per "jasd" "ddd" 23 14 13 2012 (Estudiante Astronomia 2017)) Vacia)
encolar :: Persona -> Cola -> Cola
encolar persona Vacia = Encolada persona Vacia
encolar (persona) cola = Encolada persona cola -- como hacerr para mandarlo a la ultima posicion...

--3
rolCargo ::  Cargo -> Persona -> Bool
rolCargo Titular (Per _ _ _ _ _ _ (Docente Titular)) = True
rolCargo Auxiliar (Per _ _ _ _ _ _ (Docente Auxiliar)) = True
rolCargo Adjunto (Per _ _ _ _ _ _ (Docente Adjunto)) = True
rolCargo Asociado (Per _ _ _ _ _ _ (Docente Asociado)) = True
rolCargo Asistente (Per _ _ _ _ _ _ (Docente Asistente)) = True
rolCargo _ (Per _ _ _ _ _ _ _) = False

filtraCola :: (Persona -> Bool) -> Cola -> Cola
filtraCola _ Vacia = Vacia
filtraCola funcion (Encolada persona cola)  | funcion persona == True = Encolada persona (filtraCola funcion cola)
                                            | otherwise = filtraCola funcion cola

headCola :: Cola -> Persona -- no puedo usar Maybe pq estaria cambiando la funcion busca, (quedaría Maybe Persona)
headCola Vacia = error ""  -- solo para cumplir con la pedido se hace asi.
headCola (Encolada p _) = p

busca :: Cola -> Cargo -> Persona
busca cola cargo = headCola (filtraCola (rolCargo cargo) cola)

--b El tipo Cola se parece a listas, podríamos usar listas de personas para resolver los problemas anteriores. [Persona]

-- Ejercicio 5
data ListaAsoc a b =  Vacia2 | Nodo a b (ListaAsoc a b) deriving (Show)

type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

--1 a
type GuiaTel = ListaAsoc (String,String) Int -- donde la tupla representa nombre completo y direccion y luego el telefono 

--b 1
la_long :: Integral c => ListaAsoc a b -> c
la_long Vacia2 = 0
la_long (Nodo _ _ (listaAsoc)) = 1 + la_long listaAsoc  
-- la_long (Nodo 1 2 (Nodo 2 3 (Vacia2)))

--b 2
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia2 ls1 = ls1
la_concat (Nodo a b (ls1)) ls2 = (Nodo a b (la_concat ls1 ls2))

--b 3
la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia2 = []
la_pares (Nodo a b (la)) = (a,b) : la_pares la

--b 4
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia2 _ = Nothing
la_busca (Nodo a b (la)) x | x == a = Just b
                           | otherwise = la_busca la x

--b 5
la_aListaDePares :: ListaAsoc a b -> [(a,b)]
la_aListaDePares Vacia2 = []
la_aListaDePares (Nodo a b (la)) = (a,b) : la_aListaDePares la

--b 6
la_borrar :: Eq a =>a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ Vacia2 = Vacia2
la_borrar x (Nodo a b (la)) |x == a = la
                            |otherwise = Nodo a b (la_borrar x (la))

--c Respuesta: Se puede utilizar una lista de asociaciones para resolver el ejercicio "encuentra" del proyecto anterior

--Ejercicio 6
data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)

type Prefijos = Arbol String
can , cana , canario , canas , cant , cantar , canto :: Prefijos
can = Rama cana "can" cant
cana = Rama canario "a" canas
canario = Rama Hoja "rio" Hoja
canas = Rama Hoja "s" Hoja
cant = Rama cantar "t" canto
cantar = Rama Hoja "ar" Hoja
canto = Rama Hoja "o" Hoja

--a
a_long :: Integral b => Arbol a -> b
a_long Hoja = 0
a_long (Rama (izq) _ (der)) = 1 + a_long(izq) + a_long(der)

--b
a_hojas :: Integral b => Arbol a -> b
a_hojas Hoja = 1
a_hojas (Rama (izq) _ (der)) = a_hojas(izq) + a_hojas(der)

--c
a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama (izq) x (der)) = Rama (a_inc (izq)) (x+1) (a_inc (der))

--d 
a_nombre :: Arbol Persona -> Arbol String
a_nombre Hoja = Hoja
a_nombre (Rama (izq) (Per nom ape _ _ _ _ _) (der)) = Rama (a_nombre(izq)) (ape ++ ", " ++ nom) (a_nombre(der))

--e
a_map :: (a -> b) -> Arbol a -> Arbol b
a_map _ Hoja = Hoja
a_map func (Rama izq x der) = Rama (a_map func izq) (func x) (a_map func der)

a_inc' :: Num a => Arbol a -> Arbol a
a_inc' Hoja = Hoja
a_inc' arbolito = a_map (+1) arbolito

a_nombre' :: Arbol Persona -> Arbol String
a_nombre' Hoja = Hoja
a_nombre' rama = a_map(\(Per ape nom _ _ _ _ _) -> (ape ++ ", " ++ nom)) (rama)

--f
a_sum :: Num a => Arbol a -> a
a_sum Hoja = 0
a_sum (Rama (izq) x (der)) = x + a_sum(izq) + a_sum(der)

--g
a_prod :: Num a => Arbol a -> a
a_prod Hoja = 1
a_prod (Rama (izq) x (der)) = x * a_prod(izq) * a_prod(der)



