import Test.QuickCheck

{-
1. Defina una función que obtenga la lista de los cuadrados de los números
pares de la lista de enteros de entrada. Por ejemplo, la función aplicada
sobre [2,5,8,9] devuelve [4,64]. Use listas definidas de forma intensiva.
-}
cuadradolista :: (Integral a) => [a] -> [a]
cuadradolista xs = map (^2) (filter even xs)

{-  Defina la función anterior mediante patrones y recursividad. -}

cuadradolista' :: (Integral a) => [a] -> [a]
cuadradolista' (x:[])
  | mod x 2 == 0 = [x*x]
  | otherwise = []
cuadradolista' xs
  | mod (head xs) 2 == 0 =  [(head xs) * (head xs)] ++ (cuadradolista' (tail xs))
  | otherwise = [] ++ (cuadradolista' (tail xs))

{-
3. Defina una propiedad para chequear con QuickCheck que las funciones de los dos
ejercicios anteriores son equivalentes. Chequee la propiedad conQuickCheck
-}

prop_listaMultiplos :: [Int] -> Property
prop_listaMultiplos xs = ( (length xs) >= 1) ==> (cuadradolista xs) == (cuadradolista' xs)

{-
Desde ghci compruebe la propiedad invocando
quickCheck prop_listaMultiplos
-}


{-
4. Defina una función que, dada una lista de entrada de números enteros ordenados de menor a mayor, obtenga la sublista de los números comprendidos entre dos valores dados como entrada. Por ejemplo, la sublista de los números comprendidos entre 3 y 8 de la lista de entrada [1..100] es [3,4,5,6,7,8]; la sublista de los números comprendidos entre 8 y 3 de la lista de entrada [1..100] es []; y la sublista de los números comprendidos entre 3 y 3 de la lista de entrada [1..100] es [3]. Use listas definidas de forma intensiva.
-}

rango :: [Int] -> Int -> Int -> [Int]
rango xs x y = [ z | (z,w)<- zip xs [1..], w >= x && w <= y && x <= y]

{-
 5. Defina la función anterior mediante patrones y recursividad.
-}

rango' :: [Int] -> Int -> Int -> [Int]
rango' [] x y = []
rango' xs x y
  | x > y = []
  | x > 1 = rango' (tail xs) (x-1) (y-1)
  | (length xs) >  y  = rango' (reverse (tail (reverse xs))) x y
  | otherwise = xs


{-6. Defina una propiedad para chequear con QuickCheck que las funciones de los dos
ejercicios anteriores son equivalentes. Chequee la propiedad conQuickCheck
-}

prop_rango :: [Int] -> Int -> Int -> Property
prop_rango xs x y = ( (length xs) >= 1 && x >= 0 && y >= 0 && x < y) ==> (rango xs x y) == (rango' xs x y)

{-
Desde ghci compruebe la propiedad invocando
quickCheck prop_rango
-}

{-
7. En teoría de números, los factores primos de un número entero son los números
primos divisores exactos de ese número entero. El proceso de búsqueda de esos
divisores se denomina factorización de enteros, o factorización en números primos.
Defina una función que obtenga la lista de factores primos de un número entero. Para
ello, primero defina una función que obtenga todos los divisores del número, a
continuación otra función que determine si un número es primo o no (usando la función
de los divisores), y por último defina la función de los factores primos usando las dos
funciones anteriores.
-}

obtenerDivisores :: Int -> [Int]
obtenerDivisores x = [ y | y <- [1..x], mod x y == 0 ]

isPrime :: Int -> Bool
isPrime x = length (obtenerDivisores x) == 2

factoresPrimos :: Int -> [Int]
factoresPrimos x = filter isPrime (obtenerDivisores x)

{-
8. Definir una función que dada una lista de entrada obtenga una nueva lista en la que
cada elemento de la original aparece repetido tantas veces como indica su posición. Por
ejemplo, para la lista de entrada [2,4,6] se genera la lista [2,4,4,6,6,6]; o para la cadena
de entrada “hola” se obtiene la cadena “hoolllaaaa”. Use listas definidas de forma
intensiva.
-}

repetirPosicion :: [Int] -> [Int]
repetirPosicion [] = []
repetirPosicion xs = concat [ replicate x y | (x,y) <- zip [1..] xs]

{-
Definir intensionalmente una función que dado un entero calcule una lista
con todas las triplas (x,y,z) de números entre 1 y n tales que x²+ y²= z²
-}

arq :: Int -> [(Int,Int,Int)]
arq n = [ (x,y,z) | x <- [0..n], y <- [0..n], z <- [0..n], (x^2)+(y^2) == (z^2)]
