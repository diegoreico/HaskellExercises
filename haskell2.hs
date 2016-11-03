media :: (Fractional a) => a -> a -> a
media x y = (x+y)/2

mediaGeom :: (Floating a) => a -> a -> a
mediaGeom x y = sqrt(x*y)

longitudCircuferencia :: (Floating a) => a -> a
longitudCircuferencia x = 2*pi*x

areaCircuferencia :: (Floating a) => a -> a
areaCircuferencia x = pi*x

areaEsfera :: (Floating a) => a -> a
areaEsfera x = 4*pi*x*x

ultmas2cifras :: (Integral a) => a -> a
ultmas2cifras x = mod x 100

maximo :: (Integral a) => a -> a -> a -> a -> a
maximo a b c d = max (max a b) (max c d)

comprobacionCuadrada :: (Integral a) => (a -> (a -> (a -> Bool)))
comprobacionCuadrada x y z = (x*x + y*y) == z*z

polinomio :: (Fractional a) => a -> a
polinomio x = x*x + 2*x + 1

absoluto :: (Integral a) => a -> a
absoluto x
 | x < 0 = x * (-1)
 | otherwise = x

par :: (Integral a) => a -> Bool
par x = (mod x 2) == 0

esDivisor :: (Integral a) => a -> a -> Bool
esDivisor x y = mod x y == 0 
