{--import Data.List (sortBy)

skyline :: (Integral a) => [(a,a,a)] -> [(a,a,a)]
skyline xs = sortBy compare xs
--}

-- Tipos de datos que se van a utilizar en el programa

type Edificio = (Int,Int,Int)
type Coordenada = (Int,Int)
type Silueta = [Coordenada]
{--
------------------------------------------------------------------
-- Resolución de la práctica --
------------------------------------------------------------------

-- Función principal llamada resuelve. A partir de una lista de edificios,
-- implementa el algoritmo mediante un Divide y Vencerás y devuelve la Silueta
—- Se debe implementar mediante patrones y recursividad. La función debe hacer uso 
—- de otras dos funciones: una que divide la lista Edificio en dos sublistas, 
—- para llamarse recursivamente con las subsistas, y otra que une los resultados
—- de resolver las dos sublistas separadamente.
--}
{--resuelve :: --}

{--
-- Función siluetadeedificio, que transforma un único Edificio en su Silueta correspondiente 
-- Se utiliza en el caso base del algoritmo
--}

siluetaedificio :: Edificio -> Silueta
siluetaedificio (e1,e2,e3) = [(e1,e3),(e2,0)]

{--
-- Función divide, que dado un problema (en forma de lista de Edificios) lo divide en
-- dos subproblemas cuyo tamaño es la mitad del problema original (divide y vencerás)
—- Parte de una lista y obtiene una tupla con dos listas, que son el resultado de dividir la lista de entrada en dos sublistas.
--}

{--divide :: [a] -> ([a],[a])
divide xs = ()
--}
{--
-- Función de unión que dadas dos siluetas las une para formar una única.
—- Debe tenerse en cuenta que una vez unidas las dos siluetas hay que chequear
—- que dos coordenadas consecutivas no tengan la misma altura.
--}

{--une :: Silueta -> Silueta -> Silueta
une xs ys = concat xs ys-}
{-
filtrarLista :: Silueta -> Silueta -> Silueta
filtrarLista [] _ = []
filtrarLista xs [] = xs
filtrarLista xs y:ys 
 | 
 | 
-}
filtrarElem :: Silueta -> [Coordenada] -> Silueta
filtrarElem xs [(a1, a2), (b1, b2)] = [(x, y) | (x, y) <- xs, x >= a1, x < b1, y >= a2] ++ [(x, a2) | (x, y) <- xs, x >= a1, x < b1, y < a2]

filtrarElem' (x, xs) = filtrarElem x xs 

pillarPares :: [Coordenada] -> [[Coordenada]]
pillarPares (x:xs)
 | length xs > 1 = [[x,(head xs)]] ++ (pillarPares xs)
 | otherwise = [[x, (head xs)]]

{-- 
zip (replicate 100 (1,2)) (pillarPares  [(0,2),(3,3),(5,1),(6,0)])

 --}

{--

xs'=filtrar xs con ys
ys' = filtrar ys con xs'
zs = xs' ++ ys'
zs' = eliminar(ordenador(zs))

	--}
{--
------------------------------------------------------------
-- Algoritmo para imprimir en pantalla la silueta --
------------------------------------------------------------
—- En primer lugar, se convierte la lista de coordenadas dada por la Silueta 
—- en una lista de alturas para cada coordenada x.
—- En segundo lugar, se calcula la altura máxima de la Silueta, a partir de la lista anterior.
—- En tercer lugar, se genera la línea de asteriscos para la altura máxima 
—- y descendemos hasta llegar a la altura cero. 
—- Para dibujar cada línea, en cada coordenada x se visualiza un asterisco, 
—- si la altura de la silueta en esa coordenada es mayor o igual que la altura de la línea 
—- que estamos dibujando y un espacio en blanco, si no se verifica lo anterior.
—- Finalmente, en la altura cero se visualiza una línea de guiones para indicar el suelo.
--}

