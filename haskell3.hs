import Test.QuickCheck

{-
Cree un nuevo script con la definición de dicha propiedad y compruebe la
veracidad o falsedad de dicha propiedad. Use Quickcheck para ello,
importándolo al principio del script con
-}

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

{-
Desde ghci compruebe la propiedad invocando
quickCheck prop_RevRev
-}

{-
Compruebe que el doble de la suma de dos números es la suma del doble de cada uno de ellos.
Defina la función doble de un número (véase ejemplo inicial de clase). Después defina otra
función que exprese la propiedad citada como una igualdad lógica (==) entre las dos
definiciones (el doble de la suma de dos números == suma del doble de cada número).
Compruebe la veracidad o falsedad de dicha propiedad.
-}

doble :: (Integral a) => a -> a
doble x = 2*x

prop_Doble :: Int -> Int -> Bool
prop_Doble x y = (doble x) + (doble y) == (doble (x+y))

{-
Compruebe que no se verifica la propiedad siguiente: “El cuadrado de la suma dos números
cualesquiera es igual a la suma de los cuadrados de cada número”.
-}

prop_cuadrado :: Int -> Int -> Bool
prop_cuadrado x y = x^2 + y^2 == (x+y)^2

{-
Defina la función factorial de tres formas distintas: la primera mediante recursividad usando ifs,
la segunda mediante recursividad usando casos y la tercera mediante alguna de las funciones
primitivas vistas en clase. Compruebe que las tres definiciones son idénticas para sobre los
números naturales.Como en haskell los naturales no están definidos, podemos definir las
funciones sobre los enteros y restringir la comprobación sólo a los enteros mayores o iguales
que cero. Para ello, haremos uso de las property. Definiremos la propiedad como
-}

f1 :: Int -> Int
f1 x
 | x == 0 = 1
 | otherwise = x * (f1 (x-1))

f2 :: Int -> Int
f2 x = if x == 0 then 1 else x * (f2 (x-1))

f3 :: Int -> Int
f3 x = (product [1..x])

prop_f :: Int -> Property
prop_f x = (x>=0) ==> ( f1 x == f2 x ) == ( f2 x == f3 x )


{- Funciones con patrones, recursividad y listas por comprensión  -}
{- =============================================================  -}
{-
Realizar una función que multiplique los n primeros números enteros positivos impares, haciendo
uso de listas definidas de forma intensiva (por comprensión).
2. Definir una función que, dado un número entero, obtenga una lista con los dígitos del número.
Por ejemplo, digitos’ 2678 devuelve [2,6,7,8].
-}

multNPares :: (Integral a) => a -> a
multNPares n = (product [ x | x <- [1..n*2], mod x 2 == 0])

{-
Definir una función que, dado un número entero,
obtenga una lista con los dígitos del número.
Por ejemplo, digitos’ 2678 devuelve [2,6,7,8].
-}

numerodigitos :: (Integral a) => a -> a
numerodigitos 0 = 1
numerodigitos 1 = 1
numerodigitos 2 = 1
numerodigitos 3 = 1
numerodigitos 4 = 1
numerodigitos 5 = 1
numerodigitos 6 = 1
numerodigitos 7 = 1
numerodigitos 8 = 1
numerodigitos 9 = 1
numerodigitos x = 1 + numerodigitos(div x 10)

digitos :: (Integral a) => a -> [a]
digitos n = [ mod (div n (10^(numerodigitos n - 1 - x))) 10 | x <-[0..(numerodigitos n -1)]]

{-
Definir una función que determine si un número es correctamente ordenado (es decir, sus cifras
están en orden ascendente, p.e. 138 es correctamente ordenado).
-}

ordenadok :: Int -> Bool
ordenadok x
  | (length (digitos x)) == 2 = (head (digitos x)) < (head(tail (digitos x)))
  | otherwise = head(digitos(x))<head(tail(digitos(x))) && (ordenadok(mod x (10^(numerodigitos x - 1))))

{-
4. Un número es perfecto si la suma de sus divisores (excluyéndole a él mismo) suman el propio
número. Escribir una función que determine si un número es perfecto, haciendo uso de listas
definidas de forma intensiva y patrones. Los primeros números perfectos son 1, 6, 28, 496.
-}

numeroPerfecto :: Int -> Bool
numeroPerfecto x= sum [ n | n <- [1..x-1] , mod x n == 0  ] == x

{-
5. Definir una función, usando listas intensivas, que devuelva el número formado por los dígitos de
la lista de entrada. Por ejemplo, la función devuelve 379 cuando la lista de entrada es [3,7,9]
-}

li2num :: [Int] -> Int
li2num xs= sum [ (10^z)*y | (z,y) <- zip [0..] (reverse xs) ]

{-
6. Definir una función que transforme un número entero en un nuevo número añadiéndole al entero
dado como argumento de entrada un dígito a la derecha. Por ejemplo, la function devuelve 1467
cuando se invoca con los datos 146 y 7.
-}

unir :: Int -> Int -> Int
unir x y = x*(10^length(digitos y))+y

{-
7. Definir mediante patrones una función variaciones que calcule el número
de variaciones de m elementos tomados de n en n
-}

factorial :: Int -> Int
factorial n = product [1..n]

variaciones :: Int -> Int -> Int
variaciones _ 0 = 1
variaciones 0 _ = 0
variaciones m n = div (factorial m) (factorial (m-n))

{-
8. Definir una función que concatene los dígitos de dos números positivos. Por ejemplo la
concatenación de 123 y 45 produce 12345, la de 0 y 4 produce 4, la de (-3) y 6 genera un
error. Es preferible usar guardas a ifs.
-}

unir' :: Int -> Int -> Int
unir' x y
  | x < 0 || y < 0 = error "Se ha usado un numero negativo"
  | otherwise = unir x y

{-
9. La distancia de Hamming entre dos cadenas es el número de caracteres diferentes entre las
cadenas. Por ejemplo, la distancia de Hamming entre “coma” y “rosa” es 2 (porque hay 2
caracteres que son distintos entre ambas cadenas: el primero y el tercero). Definirla
mediante listas. Guardas son preferibles a ifs.
-}

distancia :: String -> String -> [Bool]
distancia xs ys = [ x == y | (x,y) <- zip xs ys]

hamming :: String -> String -> Int
hamming [] _ = error "Lista vacia"
hamming _ [] = error "Lista vacia"
hamming xs ys
  | length (distancia xs ys) == 1 && head(distancia xs ys) == False = 1
  | length (distancia xs ys) == 1 && head(distancia xs ys) == True = 0
  | head(distancia xs ys) == False = 1 + hamming (tail xs) (tail ys)
  | head(distancia xs ys) == True = 0 + hamming (tail xs) (tail ys)

{-
Redefinir la función anterior mediante patrones.
Tenga en cuenta que los guardas se pueden
definir tras el igual del patrón:
distanciaHamming' (x:xs) (y:ys) | x==y ….
-}

hamming' :: String -> String -> Int
hamming' (x:[]) (y:[])
  | x == y = 0
  | otherwise = 1
hamming' xs ys
  | head(distancia xs ys) == False = 1 + hamming' (tail xs) (tail ys)
  | head(distancia xs ys) == True = 0 + hamming' (tail xs) (tail ys)
