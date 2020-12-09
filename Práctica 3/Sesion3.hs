-- Álvaro Corrochano López

-- Ejercicio 1

-- Last

last2 :: [a] -> a
last2 = Prelude.foldl1 (\_ x -> x)

-- Reverse

reverse2 :: [a] -> [a]
reverse2 = Prelude.foldl (\acum x -> x : acum) []

-- All

all2 :: (a -> Bool) -> [a] -> Bool
all2 f xs = foldr (\x acum -> f x && acum) True xs

-- Minimum

minimum2 :: (Ord a) => [a] -> a
minimum2 = Prelude.foldl1 (\x acum -> if x < acum then x else acum)

-- Map

map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\x acum -> f x : acum) [] xs

-- Filter

filter2 :: (a -> Bool) -> [a] ->  [a]
filter2 f xs = foldr (\x acum -> if f x then [x] ++ acum else acum ) [] xs

-- TakeWhile

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 f xs = foldr (\x acum -> if f x then x : acum else []) [] xs

-- Ejercicio 2

foldr1 :: (a -> a -> a) -> [a] -> a

foldr1 f (x:xs) = foldr f x xs

--------------------------------------

foldl1 :: (a -> a -> a) -> [a] -> a

foldl1 f (x:xs) = foldl f x xs


-- Ejercicio 3

-- a)

negpos n = foldr todos [] [1..n]
           where todos x y = x : (negate x) : y

-- b)

conjPos n = [(x, y) | x <- [0..n], y <- [0..n]]

-- Ejercicio 4

-- a)

sufijos :: [a] -> [[a]]

sufijos xs = [drop n xs | n <- [0..length xs]]

-- b)

g xs = [take n xs | n <- [1..length xs]] -- "Prefijos", como sufijos pero empieza por la izda

sublistas xs = concat(map g (sufijos xs))

-- c)

enMedio x [] = [[x]]
enMedio x (y:ys) = (x:y:ys) : [y:zs | zs <- enMedio x ys]

permuta [] = [[]]
permuta (x:xs) = concat [enMedio x ys | ys <- permuta xs]

-- d)

suma a b n r = if (b + a) < n then suma a (b + a) n (r ++ [a])
             else if (b + a) == n then r ++ [a]
                  else suma (a - 1) b n r

auxSumandos a n = if a == n then [n]
                  else suma a a n [a]

sumandos n = map (\a -> auxSumandos a n) [1..n]



















