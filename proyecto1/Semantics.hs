module Language.Semantics where

import Language.Syntax
import Language.ListAssoc

defaultIntValue :: Int
defaultIntValue = 0

defaultBoolValue :: Bool
defaultBoolValue = True

-- Tipo que representa la continuación de un paso de ejecución.
-- Ésta puede ser: Falta ejecutar una sentencia (ToExec), o ya no hay nada por
-- ejecutar (Finish).
data Continuation = ToExec Statement
                  | Finish
-- Asignación de valores para las variables enteras
type StateI = ListAssoc VarName Int
-- Asignación de valores para las variables booleanas
type StateB = ListAssoc VarName Bool

-- El estado consta del valor de las variables enteras y las booleanas
type State = (StateI,StateB)


evalIExpr :: IntExpr -> StateI -> Int
evalIExpr (ConstI a) _ = a --Constantes
evalIExpr (VI varN) lista = case la_buscar lista varN of --Variables
                            Nothing -> defaultIntValue
                            Just e -> e
evalIExpr (Neg intExpr) lista = (-1) * evalIExpr intExpr lista --Recursión hasta llegar a constante/variable y da el negativo
evalIExpr (Plus intExp1 intExp2) lista = (evalIExpr intExp1 lista) + (evalIExpr intExp2 lista) -- recursión de las dos intExpr, hasta llegar al más básico, luego lo suma.
evalIExpr (Prod intExp1 intExp2) lista = (evalIExpr intExp1 lista) * (evalIExpr intExp2 lista) -- análogo anterior
evalIExpr (Div intExp1 intExp2) lista = div (evalIExpr intExp1 lista) (evalIExpr intExp2 lista) -- siempre recursión para volver a la expresión más simple
evalIExpr (Mod intExp1 intExp2) lista = mod (evalIExpr intExp1 lista) (evalIExpr intExp2 lista)

-- Para evaluar las expresiones booleanas
-- necesitamos también el estado de variables enteras
-- porque en Equal y Less tenemos subexpresiones enteras.
evalBExpr :: BoolExpr -> State -> Bool
evalBExpr (ConstB bool) _ = bool --Simplemente la constante
evalBExpr (VB varName) (_,listaB) = case la_buscar listaB varName of
                            Nothing -> defaultBoolValue
                            Just e -> e
evalBExpr (And boolExp1 boolExp2) lista = (evalBExpr boolExp1 lista) && (evalBExpr boolExp2 lista)
evalBExpr (Or boolExp1 boolExp2) lista = (evalBExpr boolExp1 lista) || (evalBExpr boolExp2 lista)
evalBExpr (Not boolExp) lista = not (evalBExpr boolExp lista)
evalBExpr (Equal boolExp1 boolExp2) (si,_) = (evalIExpr boolExp1 si) == (evalIExpr boolExp2 si)
evalBExpr (Less boolExp1 boolExp2) (si,_) = (evalIExpr boolExp1 si) < (evalIExpr boolExp2 si) 
-- Evaluar un paso de ejecución en un programa.
evalStep :: Statement -> State -> (State , Continuation)
evalStep Skip state = (state,Finish)
evalStep (AssignB (Var n _) expBool) (si,sb) = (si,(la_agregar n (evalBExpr expBool (si,sb))sb)),Finish)
evalStep (AssignI (Var n _) expInt) (si,sb) = ((la_agregar n (evalIExpr expInt si) si,sb),Finish)
evalStep (Seq statement1 statement2) state = case evalStep statement1 state of 
                                             (state2,Finish) -> (state2,ToExec statement2)
                                             (state2, ToExec stmnt1') -> (state2, ToExec (Seq stmnt1' statement2))
-- Para la secuencia, evalua primero una de ellas y se fija que devuelve, si está terminada la primera entonces pasa a la 
-- segunda si no la termino lo que hace es ejecutar nuevamente la sentencia con los pasos que faltan, una vez terminada
-- lo que hace es pasar a la segunda.
evalStep (If []) _ = error "mal"
evalStep (If [(boolExpr,statement)]:xs) state | (evalBExpr boolExpr state) = (state,ToExec stmnt)
                                              | not(evalBExpr boolExpr state) = (state, ToExec (If xs))
                                              | otherwise = error "mal"
-- Nos preguntamos si se cumple la expresion booleana, si se cumple, ejecutamos el statement, si no se cumple, pasamos
-- a la siguiente guarda y asi siguiendo
evalStep (Do boolExpr statement) state | (evalBExpr boolExpr state) = (state, ToExec (Seq statement (Do boolExpr statement))
                                       | not (evalBExpro boolExpr state) = (state,Finish)
                                       | otherwise error "mal"
-- En el do, nos fijamos si se cumple la guarda, si es así, lo que se hace es ejecutar el "cuerpo" del ciclo, si no
-- se cumple, entonces debemos salir del ciclo y finalizar su ejecución.



