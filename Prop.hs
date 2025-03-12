module Prop where
-- Profesor: Manuel Soto Romero
-- Ayudante: Diego Méndez Medina
-- Ayudante: José Alejandro Pérez Marquez
-- Laboratorio: Erick Daniel Arroyo Martínez
-- Laboratorio: Erik Rangel Limón

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

instance Show Prop where
    show :: Prop -> String
    show = undefined
    
    
-- ------------------------------------------------------------------------------
-- Ejercicio 2
-- Definir la funcion conjPotencia, tal que la aplicación de la funcion es la
-- lista de todos los subconjuntos de x.
-- ------------------------------------------------------------------------------
conjPotencia :: [a] -> [[a]]
conjPotencia = undefined

-- ------------------------------------------------------------------------------
-- Ejercicio 3.
-- Definir la función vars::Prop -> [String] que devuelve el conjunto de variables
-- proposicionales de una fórmula.
-- ------------------------------------------------------------------------------

vars :: Prop -> [String]
vars = undefined

-- ------------------------------------------------------------------------------
-- Ejercicio 4.
-- Definir la función interpreta que dada una formula proposicional y un estado
-- regrese la interpretación obtenida de la fórmula en dicho estado.
-- ------------------------------------------------------------------------------
    
interpretacion :: Prop -> Estado -> Bool
interpretacion = undefined

-- ------------------------------------------------------------------------------
-- Ejercicio 5.
-- Definir la funcion modelos :: Prop -> [Estado] que dada una fórmula devuelve
-- una lista de estados que satisfacen a dicha fórmula.
-- ------------------------------------------------------------------------------
modelos :: Prop -> [Estado]
modelos = undefined

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
