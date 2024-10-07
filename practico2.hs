{-
Actividad 3:
(2^29)/(2^9) == (2^20)

Actividad 4:
(head(drop 3) "0123456")
head "3456"
3 Tipo Int

Actividad 5:
:t True && False
:t sqrt 16
:t 5 == 5
:t (2: [])
:t (++)
:t length [5,4,3,2,1]
-}

--Actividad 6. Producto escalar de dos vectores.:
--Versión 1:
prodEscalar :: (Int, Int) -> (Int, Int) -> Int
prodEscalar (x1, y1) (x2, y2) = x1*x2 + y1*y2

--Versión 2:
prodEscalar' :: (Int, Int) ->(Int, Int) -> Int
prodEscalar' (x1, y1) (x2, y2) = fst(x1, y1) * fst(x2, y2) + snd(x1, y1) * snd(x2, y2)

--Actividad 7:
--Versión 
abs :: Int -> Int
abs x = if x >= 0 then x else (-1)*x

--Actividad 8 (Definición por pattern matching):
sumLista :: [Int] -> Int
sumLista [] = 0 --Caso base
sumLista (x:xs) = x + sumLista xs
--Con foldr:
sumListaFoldr :: [Int] -> Int
sumListaFoldr = foldr (+) 0

--Actividad 9:
--potencia :: (Int, Int) -> Int Sin currificar
--potencia (x, y) = fst(x, y) ^ snd(x, y) 
potencia :: Int -> Int -> Int
potencia x y = x^y

--Actividad 10 (En base a 9):
potDos :: Int -> Int
potDos x = potencia 2 x

--Actividad 11:
sumTres :: Int -> Int -> Int -> Int
sumTres x y z = x + y + z

--Actividad 12 (En base a 11):
sumCuatro :: Int -> Int -> Int
sumCuatro x y = sumTres 4 x y

{-
Actividad 13: (Aclaración, con "arreglo" se refiere a lista)
Analizar el programa que dado un arreglo y la cantidad de elementos a explorar, retorna True si todos los elementos del mismo 
son 0. False en caso contrario.
-}

zeros :: Num a => Eq a => [a] -> Int -> Bool --Perfil
zeros a 0 = True
zeros a n = zeros a (n-1) && (a!!(n-1) == 0)

{-
¿Qué resultado arrojará dicho programa si lo ejecutamos con las siguientes entradas?:
(a) > zeros [0,2,0] 3 -False
(b) > zeros [0,0,3] 2 -True (No toma el espacio 2 que no es 0, porque empieza en el espacio (2-1) (1))
(c) > zeros [0,0,0,0] 4 - True
-}

--El siguiente código implementa la misma funcionalidad, es decir retorna True si todos los elementos del mismo son 0.

zeroes :: Num a => Eq a => [a] -> Bool
zeroes [] = True
zeroes (a:as) = zeroes as && (a == 0)

{-
¿Qué diferencia tiene? ¿Cómo es el perfil de ambas funciones?. De
un ejemplo de invocación de la misma.
Diferencias:
    - Parámetros: 
    zeros toma una lista y un Int.
    zeroes toma una lista.
    - Análisis de posiciones:
    zeros analiza desde la posición pasada -1 hasta la 0.
    zeroes analiza todas las posiciones.
    - Caso base:
    zeros devuelve True si se pasa una lista y 0 (posición inexistente)
    zeroes devuelve True si se pasa una lista vacía
    - Análisis:
    zeros verifica cada posición por pattern matching Y que la posición sea igual a cero.
    zeroes verifica la cola de la lista por pattern matching Y que la cabeza sea igual a cero.

    Ejemplo de zeroes: 
    zeroes [0,0,0,1]
    zeroes 0:[0,0,1] a==0
    zeroes 0:(0:[0,1]) a==0
    zeroes 0:(0:(0:[1])) a==0
    zeroes 0:(0:(0:(1):[])) a!=0
    False

    Ejemplo de zeros:
    zeros [0,0,2] 2
    zeros (2-1) 
-}

--Actividad 14:
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

--Actividad 15:
belongs :: Eq a => a -> [a] -> Bool
belongs e [] = False
belongs e (a:as) = belongs e as || (a == e)

{-
    Ejemplos:
    (1) belongs 5 [1,2,5,4,3]
    belongs 5 [2,5,4,3] || (1 != 5) 
    belongs 5 [5,4,3] || (2 != 5)
    belongs 5 [4,3] || (5 == 5)
    True 

    (2) belongs 's' ['a','b','c']
    belongs 's' ['b','c'] || ('a' != 's')
    belongs 's' ['c'] || ('b' != 's')
    belongs 's' [] || ('c' != 's')
    False || False
    False
-}

--Actividad 16:
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--Actividad 17:
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

--Aparte:
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (x:xs) = drop (n-1) xs