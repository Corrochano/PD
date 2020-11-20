-- Álvaro Corrochano López DNI: 02711642B
-- Práctica 2


-- Ejercicio 1

-- a)

cuadrados :: Int -> [Int]

cuadrados x = if x /= 0 then cuadrados (x-1) ++ [x^2]
                else [0]

-- b) 

cuadrados' :: Int -> [(Int, Int)]

cuadrados' y = if y /= 0 then [(y,y^2)] ++ (cuadrados' (y-1))
                else [(0,0)]


-- c)

sumcos :: Float -> Float

sumcos(1) = abs(cos(1)) 
sumcos(n) = (n * abs(cos(n))) + sumcos(n-1)

-- d)

sumult :: Int -> Int

sumult(0) = 0
sumult(1) = 0
sumult(2) = 0
sumult(3) = 0

sumult(n) = if ((n - 1) `mod` 3 == 0) || ((n - 1) `mod` 5 == 0) then (n-1) + sumult(n - 1)
            else sumult(n - 1)


-- Extra, a) con guardas en lugar de if
cuadrados3 :: Int -> [Int]

cuadrados3 x
         | (x == 0) = [0]
         | otherwise = cuadrados3 (x-1) ++ [x^2]

-- Ejercicio 2

-- a)

cuadrados2 :: Int -> [Int]

cuadrados2 n = map(^2) [0..n]

-- b)

cuadrados2' :: Int -> [(Int,Int)]

cuadrados2' n = reverse(zip [0..n] (map(^2) [0..n]))

-- c) 

sumcos2 :: Float -> Float

sumcos2 x = sum (map (\f -> f * abs(cos(f))) [1..x])

-- Ejercicio 3

-- a) 

iguales :: (Enum a, Eq b) => (a -> b) -> (a -> b) -> a -> a -> Bool

iguales f g n m = all (\x -> f(x) == g(x)) [n..m]

-- b)
menorA ::  Enum a => a -> a -> (a -> Bool) -> a

menorA n m p = head(filter p [n..m])

-- c)
mayorA :: Enum a => a -> a -> (a -> Bool) -> a

mayorA n m p = head(reverse(filter p [n..m]))

-- d)
ex :: Enum a => a -> a -> (a -> Bool) -> Bool

ex n m p = if length (filter p [n..m]) /= 0 then True
           else False

-- Ejercicio 4

-- a)

filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])

filter2 xs p q = (us, vs)
                 where us = filter p xs
                       vs = filter q xs

-- b)

filters :: [a] -> [(a -> Bool)] -> [[a]]

filters xs ps = map(\p -> filter p xs) ps


