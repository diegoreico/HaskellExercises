intercambiar :: (a -> b -> c) -> (b -> a -> c)
intercambiar f x y = f y x

suma :: (Num a) => a -> a -> a
suma x y = x+y


{- podese facer porque estan as funcións currificadas-}

sumaelem :: (Num a) => a -> [a] -> [a]
sumaelem x xs = map (suma x) xs

sumaelem' :: (Num a) => a -> [[a]] -> [[a]]
sumaelem' x xss = map (sumaelem x) xss

sumaelem'' :: (Num a) => a -> [[a]] -> [[a]]
sumaelem'' x xss = map (map (suma x)) xss

cuadrado :: (Num a) => [[a]] -> [[a]]
cuadrado xss = map(map(^2)) xss

mayorDivisible :: (Integral a) => a -> a
mayorDivisible y = last [ x | x <- [1..300000], (mod y 11) == 0 ]

{- non furrula -}
mayorDivisible' :: (Integral a) => a -> a
mayorDivisible' x = head ( filter p [300000,299999] )
  where p y = mod y x == 0


{- ========================================================================== -}
