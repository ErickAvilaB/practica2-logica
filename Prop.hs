module Prop where
-- Profesor: Manuel Soto Romero
-- Ayudante: Diego Méndez Medina
-- Ayudante: José Alejandro Pérez Marquez
-- Laboratorio: Erick Daniel Arroyo Martínez
-- Laboratorio: Erik Rangel Limón

import Data.List (nub) -- nub elimina duplicados de una lista

-- ------------------------------------------------------------------------------
-- Definicion de los tipos de datos siguientes:
-- Prop para representar las fórmulas proporsicionales usando los
-- constructores T, F, Var, Neg, Conj, Disy, Impl y Equiv para formulas atomicas,
-- negaciones, conjunciones, implicaciones y equivalencias respectivamente.
-- ------------------------------------------------------------------------------

data Prop = T | F | Var String
          | Neg Prop
          | Conj Prop Prop | Disy Prop Prop
          | Impl Prop Prop | Equiv Prop Prop deriving Eq

type Estado = [String]

-- ------------------------------------------------------------------------------
-- Ejercicio 1.
-- Definir un ejemplar de la clase Show para el tipo de dato Prop que muestre una
-- cadena que represente las formulas proposicionales en notacion infija.
-- ------------------------------------------------------------------------------

-- Función auxiliar que indica si una fórmula es atómica (T, F o variable)
isAtomic :: Prop -> Bool
isAtomic T = True
isAtomic F = True
isAtomic (Var _) = True
isAtomic _ = False

-- Función auxiliar que indica si una fórmula es un conectivo binario
isBinary :: Prop -> Bool
isBinary (Conj _ _) = True
isBinary (Disy _ _) = True
isBinary (Impl _ _) = True
isBinary (Equiv _ _) = True
isBinary _ = False

-- Función que remueve los paréntesis externos de una cadena
removeOuterParens :: String -> String
removeOuterParens s =
  if length s >= 4 && head s == '(' && last s == ')'
    then drop 2 (take (length s - 2) s)
    else s

instance Show Prop where
    -- show :: Prop -> String
    show T = "T"
    show F = "F"
    show (Var s) = s
    show (Neg p)
      -- Si el operando es atómico se muestra directamente (ej. ¬q)
      | isAtomic p = "¬" ++ show p
      -- Si es una fórmula binaria se remueven los paréntesis que ya genera su show
      | isBinary p = "¬( " ++ removeOuterParens (show p) ++ " )"
      -- Para otros casos (por ejemplo, doble negación) se coloca la fórmula entre paréntesis
      | otherwise = "¬(" ++ show p ++ ")"
    show (Conj p q) = "( " ++ show p ++ " /\\ " ++ show q ++ " )"
    show (Disy p q) = "( " ++ show p ++ " \\/ " ++ show q ++ " )"
    show (Impl p q) = "( " ++ show p ++ " -> " ++ show q ++ " )"
    show (Equiv p q) = "( " ++ show p ++ " <-> " ++ show q ++ " )"


-- ------------------------------------------------------------------------------
-- Ejercicio 2
-- Definir la funcion conjPotencia, tal que la aplicación de la funcion es la
-- lista de todos los subconjuntos de x.
-- ------------------------------------------------------------------------------
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x : xs) =
  let subs = conjPotencia xs
   in subs ++ map (x :) subs

-- ------------------------------------------------------------------------------
-- Ejercicio 3.
-- Definir la función vars::Prop -> [String] que devuelve el conjunto de variables
-- proposicionales de una fórmula.
-- ------------------------------------------------------------------------------

vars :: Prop -> [String]
vars T = []
vars F = []
vars (Var s) = [s]
vars (Neg p) = vars p
vars (Conj p q) = nub (vars p ++ vars q)
vars (Disy p q) = nub (vars p ++ vars q)
vars (Impl p q) = nub (vars p ++ vars q)
vars (Equiv p q) = nub (vars p ++ vars q)

-- ------------------------------------------------------------------------------
-- Ejercicio 4.
-- Definir la función interpreta que dada una formula proposicional y un estado
-- regrese la interpretación obtenida de la fórmula en dicho estado.
-- ------------------------------------------------------------------------------

interpretacion :: Prop -> Estado -> Bool
interpretacion T _ = True
interpretacion F _ = False
interpretacion (Var s) st = s `elem` st
interpretacion (Neg p) st = not (interpretacion p st)
interpretacion (Conj p q) st = interpretacion p st && interpretacion q st
interpretacion (Disy p q) st = interpretacion p st || interpretacion q st
interpretacion (Impl p q) st = not (interpretacion p st) || interpretacion q st
interpretacion (Equiv p q) st = interpretacion p st == interpretacion q st

-- ------------------------------------------------------------------------------
-- Ejercicio 5.
-- Definir la funcion modelos :: Prop -> [Estado] que dada una fórmula devuelve
-- una lista de estados que satisfacen a dicha fórmula.
-- ------------------------------------------------------------------------------
modelos :: Prop -> [Estado]
modelos p = filter (interpretacion p) (conjPotencia (vars p))

-- ------------------------------------------------------------------------------
-- Ejercicio 6.
-- Definir una función que dada una fórmula proposicional, indique si es una 
-- tautologia.
-- Firma de la funcion: tautologia:: Prop -> Bool
-- ------------------------------------------------------------------------------

tautologia :: Prop -> Bool
tautologia = undefined

-- ------------------------------------------------------------------------------
-- Ejercicio 7.
-- Definir una funcion que dada una fórmula proposicional, indique si es una
-- contradicción.
-- firma de la funcion: contradiccion :: Prop -> Bool
-- ------------------------------------------------------------------------------
contradiccion :: Prop -> Bool
contradiccion = undefined

-- ------------------------------------------------------------------------------
-- Ejercicio 8.
-- Definir una función que dada una fórmula proposicional phi, verifique si es 
-- satisfacible.
-- ------------------------------------------------------------------------------
esSatisfacible :: Prop -> Bool
esSatisfacible = undefined

-- ------------------------------------------------------------------------------
-- Ejercicio 9.

-- Definir una función que elimine dobles negaciones y aplique las
-- leyes de DeMorgan dada una fórmula proposicional phi.
-- ------------------------------------------------------------------------------
deMorgan :: Prop -> Prop
deMorgan = undefined

-- ------------------------------------------------------------------------------
-- Ejercicio 10.
-- Definir una función que elimine las implicaciones lógicas de una proposición
-- ------------------------------------------------------------------------------
elimImplicacion :: Prop -> Prop
elimImplicacion = undefined

-- ------------------------------------------------------------------------------
-- Ejercicio 11.
-- Definir una funcion que elimine las equivalencias lógicas de una proposición.
-- ------------------------------------------------------------------------------
elimEquivalencias :: Prop -> Prop
elimEquivalencias = undefined

-- ------------------------------------------------------------------------------
-- Número de pruebas a hacer.
-- Puedes cambiar este valor siempre y cuando éste sea mayor o igual a 100.
-- ------------------------------------------------------------------------------
pruebas :: Int
pruebas = 1000
