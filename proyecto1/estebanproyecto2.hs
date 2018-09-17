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

--e) Se podria hacer con sinónimos de tipo, pero ¿Cómo?

--f) Para representar un alumno que está inscripto en dos carreras podríamos hacer un sinónimo de 
-- tipo en el que la parte del constructor de carrera se pase una tupla con las dos carreras, en 
-- el constructor

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

--b)Por definiciones recursivas por ahí si se puede.

--c) 
--1 
edad :: Persona -> (Int,Int,Int) -> Int
edad (Per _ _ _ dia mes ano _) (d,m,a) = abs(div ((d-dia) + ((m-mes) * 30) + ((a-ano) * 365)) 365)

--2
existe :: String -> [Persona] -> Bool
existe _ [] = False
existe apellido (Per ape _ _ _ _ _ _ : xs) = apellido == ape || existe apellido xs
--existe "airasca" [Per "jeje" "petrone" 23 14 13 2012 (NoDocente Administrativa),Per "airasca" "esteban" 23 14 13 2012 (NoDocente Administrativa),Per "bon jovi" "jon" 23 14 13 2012 (NoDocente Administrativa) ,Per "quito" "esteban" 23 14 13 2012 (NoDocente Administrativa)]

--3
{--est_astronomia' :: [Persona] -> [Persona]
est_astronomia' [] = []
est_astronomia' ((Per nom ape doc d m a rol:xs))| rol == (Estudiante Astronomia _) = (Per nom ape doc d m a rol) : est_astronomia xs
                                              | otherwise = est_astronomia xs--}
--est_astronomia [Per "jeje" "petrone" 23 14 13 2012 (Estudiante Astronomia 2017),Per "airasca" "esteban" 23 14 13 2012 (Estudiante Matematica 2015),Per "bon jovi" "jon" 23 14 13 2012 (NoDocente Administrativa) ,Per "quito" "esteban" 23 14 13 2012 (Estudiante Astronomia 2017),Per "julio" "juli" 23 14 13 2012 Decano]

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
-- atender (Encolada (Per "jeje" "petrone" 23 14 13 2012 (Estudiante Astronomia 2017)) (Encolada (Per "jej" "asd" 23 14 13 2012 (Estudiante Astronomia 2017)) Vacia))
atender :: Cola -> Cola
atender Vacia = Vacia
atender (Encolada _ cola) = cola

--2
-- encolar (Per "jeje" "petrone" 23 14 13 2012 (Estudiante Astronomia 2017)) Vacia
-- encolar (Per "jeje" "petrone" 23 14 13 2012 (Estudiante Astronomia 2017)) (Encolada (Per "jasd" "ddd" 23 14 13 2012 (Estudiante Astronomia 2017)) Vacia)
encolar :: Persona -> Cola -> Cola
encolar persona Vacia = Encolada persona Vacia
encolar (persona) cola = Encolada persona cola -- como hacerr para mandarlo a la ultima posicion...

--3
busca :: Cola -> Cargo -> Persona
busca Vacia _ = error "que devolver?"
busca Encolada cargo = 

