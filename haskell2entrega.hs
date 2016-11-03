{-
Defina una función que modele la implicación lógica x => y, que es falsa si x es cierto e y falso, y que es cierta en cualquier otro caso. Debe definirla utilizando ifs.
-}

implica :: Bool -> Bool -> Bool
implica x y = if x == True && y == False then False else True

{-
Defina la función anterior de modo que se pueda utilizar como un operador en modo infijo (p.e True `imp` False)
-}

{-
True `implica` False
-}

{-
Defina la función anterior por casos, es decir, sustituyendo la estructura de control if por ecuaciones ‘guards’.
-}

implica2 :: Bool -> Bool -> Bool
implica2 x y
 | x == True && y == False = False
 | otherwise = True


{-
Defina una función que dado un número de mes (1:Enero, 12:Diciembre) devuelva el número de días que tiene. Suponga que el año no es bisiesto. Defina la función mediante casos
-}

dias :: (Integral i) => i -> i
dias x
 | x == 2 = 28
 | mod x 2 == 0 && x > 0 && x <= 6 = 30
 | mod x 2 /= 0 && x > 0 && x <= 6 = 31
 | mod x 2 == 0 && x > 6 && x <= 12 = 31
 | mod x 2 /= 0 && x > 6 && x <= 12 = 30
 | otherwise = -1

{-
Amplíe la función anterior para contemplar el caso de año bisiesto,
definiendo la función mediante casos. Para ello, averigüe cuál es
el algoritmo para identificar un año bisiesto. La función debe recibir
dos números, el correspondiente al día de mes y al año para el cuál se
devuelve el número de días que tiene un mes concreto en ese año.
El resultado debe ser True para 2000 y 2004, y False para 2010 y 2100
-}

dias2 :: (Integral i) => i -> i -> i
dias2 x y
 | x == 2 && (( mod y 4 == 0 && mod y 100 /= 0) || mod y 400 == 0 )  = 29
 | x == 2 = 28
 | mod x 2 == 0 && x > 0 && x <= 6 = 30
 | mod x 2 /= 0 && x > 0 && x <= 6 = 31
 | mod x 2 == 0 && x > 6 && x <= 12 = 31
 | mod x 2 /= 0 && x > 6 && x <= 12 = 30
 | otherwise = -1


{-
Defina una función mediante casos que reciba dos ternas de números y calcule
la edad de una persona. Cada terna representa una fecha (día, mes, año) y
la primera es la fecha de nacimiento de una persona y la segunda es la fecha
actual. La función debe producir como resultado la edad de la persona en años.
Por ejemplo, si la persona ha nacido el día 24 del 12 de 1988, tiene
actualmente 26 años, pero si ha nacido exactamente el día de hoy de 1988,
hoy cumpliría 27 años
-}

edad :: (Integral i) => (i,i,i) -> (i,i,i) -> i
edad (a,b,c)(x,y,z)
 | y >= b || ( y == b && x>=a )= z-c
 | otherwise = z-c-1

{-
Defina una función mediante casos que reciba una fecha dada por tres números
(día, mes y año), y produzca una cadena de caracteres con la fecha en formato
texto: p.e. 16 de Diciembre de 2005. Para ello, será necesario utilizar la
función show así como el operador ++ de concatenación de cadenas. Indague
qué hace la función show
-}
{-
show -> transforma el valor de una variable a su representaión en base a caracteres
-}

fechatexto :: (Num a,Show a,Eq a) => a -> a -> a -> String
fechatexto x y z
 | y == 1 = (show x) ++ " de Enero de " ++ (show z)
 | y == 2 = (show x) ++ " de Febrero de " ++ (show z)
 | y == 3 = (show x) ++ " de Marzo de " ++ (show z)
 | y == 4 = (show x) ++ " de Abril de " ++ (show z)
 | y == 5 = (show x) ++ " de Mayo de " ++ (show z)
 | y == 6 = (show x) ++ " de Junio de " ++ (show z)
 | y == 7 = (show x) ++ " de Julio de " ++ (show z)
 | y == 8 = (show x) ++ " de Agosto de " ++ (show z)
 | y == 9 = (show x) ++ " de Septiembre de " ++ (show z)
 | y == 10 = (show x) ++ " de Octubre de " ++ (show z)
 | y == 11 = (show x) ++ " de Noviembre de " ++ (show z)
 | y == 12 = (show x) ++ " de Diciembre de " ++ (show z)


{-.
Defina una función recursiva que sume los n primeros números enteros positivos pares. Utilice la definición por casos.
-}

sumaNPares :: (Integral a) => a -> a
sumaNPares x
 | x == 0 = 0
 | otherwise = sumaNPares (x-1) + x*2


{-
Defina función recursiva que determine si un número entero de cuatro cifras es capicúa. Asuma que el número siempre tiene cuatro cifras (no es necesario generalizar la solución para distintos números de cifras). No se debe utilizar funciones de manipulación de Strings.
-}

numerodigitos :: (Integral a) => a -> a
numerodigitos x
 | x < 10 = 1
 | otherwise = 1 + numerodigitos(div x 10)


checkCapicua :: (Integral a) => a -> Bool
checkCapicua x
 | x < 10 = True
 | x < 100 = mod x 11 == 0
 | otherwise = checkCapicua( div (x - mod x (10^(numerodigitos x -1))) (10^(numerodigitos x -2)) + mod x 10
 ) && checkCapicua( div (mod x (10^(numerodigitos x -1))) 10 )

{-

{-----------------------------------

(B,L,Ca,Col) -> cada elemento I|D

Estado inicial -> (I,I,I,I)
Estado final -> (D,D;D,D)

operadores:
- op1: barquero
	- precondiciones
		- L y Ca no estan en la misma orilla
		- Ca y Col no estan en la misma orilla
	-postcondiciones
		- (I,_,_,_) -> (D,_,_,_)
		- (D,_,_,_) -> (I,_,_,_)

- op2: barquero lobo
	- precondiciones
		- cabra y col no estan en orillas diferentes
		- barquero y elemento en la misma orilla
	-postcondiciones
		- (I,I,_,_) -> (D,D,_,_)
		- (D,D,_,_) -> (I,I,_,_)

- op3: barquero col
	- precondiciones
		- L y Ca orillas distintas
		- barquero y elemento en la misma orilla
	-postcondiciones
		- (I,_,_,I) -> (D,_,_,D)
		- (D,_,_,D) -> (I,_,_,I)

- op4: barquero cabra
	- precondiciones
		- col y lobo orillas distintas
		- barquero y elemento en la misma orilla
	-postcondiciones.goo
		- (I,_,I,_) -> (D,_,D,_)
		- (D,_,D,_) -> (I,_,I,_)


(I,I,I,I) ->
	 Ca & B -> (D,I,D,I) ->
			B -> (I,I,D,I)
				B & Col -> (D,I,D,D)
					B & Ca -> (I,I,I,D)
						B & Col ->
						B 6 Ca ->
					B & Col -> (I,I,D,I)
				B & L -> (D,D,I,D)
					B & L -> (I,I,D,I)
			Ca & B -> (I,I,I,I)





-----------------------------------

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial 2 = 2
factorial n = n * factorial(n-1)

sumaV :: (Integral a) => (a,a) -> (a,a) -> (a,a)
sumaV (b,c)(d,e) = (b+d,c+e)

fsttripla :: (a,a,a) -> a
fsttripla (x,_,_) = x

checkchar :: [Char ] -> Bool
checkchar ['a',_,_] = True
checkchar _ = False

check :: [a ] -> a
check ( _:_ ) = False
check _ = True

  {- NON FUNCIONAR NINGUN
    vacia :: (Show a) => [a] -> String
    vacia [] = "La Lista esta vacia"
    vacia (x:[]) = "La lista tiene un elemento " ++ (show x)
    vacia (x:_) = "La lista tiene más de un elemento, y el primero es " ++ (show x)

    resto :: [a] -> [b]
    resto (_:xs) = xs
  -}

-}

tam :: [a] -> Int
tam [] = 0
tam (_:xs) = 1+ tam(xs)
-}
