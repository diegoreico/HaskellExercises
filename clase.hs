{- Definición de conjuntos -}
obterPares :: (Integral a) => [a] -> [a]
obterPares ys = [ x | x <- ys, mod x 2 == 1, x<10 ]

productoLista :: (Integral a) => [a] -> [a] -> [a]
productoLista ys zs = [ i*j | i <- ys, j <- zs ]

minusculas :: [Char] -> [Char]
minusculas ys = [ y | y <- ys, elem y ['a'..'z'] ]

{-
adyacentes :: [Integral] -> [(Integral,Integral)]
adyacentes ys = zip ys (tail ys)
-}
