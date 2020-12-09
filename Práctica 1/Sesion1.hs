-- Álvaro Corrochano López

-- Ejercicio 1

-- a)

anioendiez = div (10 ^ 6) 31536000

-- b)
segundosdiez = 10 ^ 6
minutosdiez = div segundosdiez 60
horasdiez = div minutosdiez 60
diasdiez = div horasdiez 24
aniosdiez = div diasdiez 365

segundosrest10 = segundosdiez `mod` 60
minutosrest10 = minutosdiez `mod` 60
horasrest10 = horasdiez `mod` 24
diasrestdiez = diasdiez `mod` 365

todoendiez = [aniosdiez, diasrestdiez, horasrest10, minutosrest10, segundosrest10]

-- c)
seganios :: Int -> Int

seganios(x) = div x 31536000

todoenx :: Integral a => a -> (a,a,a,a,a)

todoenx x = (((((x `div` 60) `div` 60) `div` 24) `div` 365), (((x `div` 60) `div` 60) `div` 24) `mod` 365, ((x `div` 60) `div` 60) `mod` 24, (x `div` 60) `mod` 60 , x `mod` 60)

-- Ejercicio 2

todos :: [Float] -> Float

todos[] = 0
todos(x:xs) = x + todos xs

---------------------------------------

media :: [Float] -> Float

media[] = 0
media(xs) = (todos xs) / (fromIntegral(length xs)) -- todos se puede sustituir por sum

media2 :: [Float] -> Float

media2[] = 0
media2(xs) = (sum xs) / (fromIntegral(length xs))

-- Ejercicio 3

-- a)

digitos :: Integer -> Integer

digitos(0) = 0
digitos(x) = 1 + digitos (div x 10)

-- b)

reduccion :: Integer -> Integer

reduccion(0) = 0
reduccion(x) = (x `mod` 10) + reduccion(div x 10)

--c)


-- Ejercicio 4

con1 :: Bool -> Bool -> Bool

con1 x y = if x == False 
                   then False
                   else if y == False
                        then False
                        else True

con2 :: Bool -> Bool -> Bool

con2 x y = if y == False 
                   then False
                   else if x == False
                        then False
                        else True 

con3 :: Bool -> Bool -> Bool

con3 x y = if ((x == False) || (y == False))
           then False
           else True

con4 :: Bool -> Bool -> Bool

con4 x y
     | ((x == False) || (y == False)) = False
     | otherwise = True

-- Ejercicio 5

{-

last [1..10^5] Poco
last [1..10^7] Regular
last [1..10^20] Mucho
head [1..10^20] Poco
1 Poco
last [10^20..1] 
head (tail [1..10^20])
length [1..10^20]
last (take (10^7) [1..10^20])
head (take (10^7) ([1..100] ++ [1..10^20]))
last (take 100 ([1..10^20] ++ [1..100]))
last (drop 100 ([1..10^20] ++ [1..100]))
head (drop (10^7) ([1..10^20] ++ [1..100]))
[1..10^7]==[1..10^7]
[1..10^20]==[1..10^20]
[1..10^20]==[1..10^20+1]
[1..10^20]==[2..10^20]
head (reverse [1..10^7])
last (reverse [1..10^7])
reverse [1..10^20] == reverse [1..10^20+1]


-}




