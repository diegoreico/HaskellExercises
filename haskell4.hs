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
