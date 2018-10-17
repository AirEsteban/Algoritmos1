module Language.ListAssoc where

data ListAssoc a b = Empty
                   | Node a b (ListAssoc a b)
                   
-- CONSGINA (3): tarea de este ejercicio es completar el archivo ListAssoc.hs con las operaciones necesarias
--para manipular las listas de asociaciones.                   

la_long :: Integral c => ListAssoc a b -> c
la_long Empty = 0
la_long (Node _ _ la) = 1 + la_long la 

la_aListaDePares :: ListAssoc a b -> [(a,b)]
la_aListaDePares Empty = []
la_aListaDePares (Node a b la) = (a,b) : la_aListaDePares (la)

la_existe :: Eq a => ListAssoc a b -> a -> Bool
la_existe Empty _ = False
la_existe (Node a _ (ls)) x = (a==x) || la_existe ls x

la_buscar :: Eq a => ListAssoc a b -> a -> Maybe b 
la_buscar Empty _ = Nothing
la_buscar (Node a b ls) x | x == a = Just b
                          | otherwise = la_buscar (ls) x

la_agregar :: Eq a => a -> b -> ListAssoc a b -> ListAssoc a b
la_agregar x y ls  | la_existe ls x = la_agregar x y (la_borrar x ls)  
                   | otherwise = Node x y (ls) 

la_borrar :: Eq a => a -> ListAssoc a b -> ListAssoc a b 
la_borrar _ Empty = Empty
la_borrar x (Node a b ls) | x == a = ls
                          | otherwise = Node a b (la_borrar x ls)





