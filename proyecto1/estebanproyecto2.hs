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
data Persona = Per String String Int Int Int Int Rol
--data Per1 = Per1 String String Int Int Int Int Rol

--b)Por definiciones recursivas por ahí si se puede.

--c) 
edad :: Persona -> (Int,Int,Int) -> Int
edad Persona 

        

             
