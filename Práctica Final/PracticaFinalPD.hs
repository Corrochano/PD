-- Práctica Final Programación Declarativa
-- Álvaro Corrochano López

-- Declaración de constantes

r1 = R [(1,2),(3,4),(5,6),(1,6)]
r2 = R [(2,1),(6,2),(4,3),(2,4),(6,5)]
r3 = R [(3,4),(1,2),(1,6),(5,6)]
r4 = R [(1,2),(2,1),(1,1),(2,2)]
r5 = R [(1,2),(3,4),(5,6),(1,6),(2,7)]
r6 = R [(10,25),(12,11),(10,10),(25,20)]
r7 = R [(1,2),(2,1),(1,1),(2,2),(7,7),(4,4),(7,4),(4,7)]

-- 1.

data Rel a = R [(a,a)] deriving (Show, Read)

esRelacion :: Ord a => Rel a -> Bool

esRelacion (R []) = True
esRelacion (R (x:r)) = if elem x r 
                           then False
                           else esRelacion(R r)

-- 2.

esta :: Ord a => Rel a -> Rel a -> Bool -- Función para comprobar si los elementos de una relación están en los de otra

esta (R []) ys = True
esta xs (R []) = False
esta (R xs) (R ys) = foldl (&&) True (map (\f -> elem f ys) xs)

instance Ord a => Eq (Rel a) where
   (==) (R []) (R ys) = True
   (==) (R xs) (R []) = True
   (==) (R xs) (R ys) = length xs == length ys && esta (R xs) (R ys) && esta (R ys) (R xs)  

-- 3.

-- dominio r = conjunto dominio de la relacion r.

quitaDupli :: (Ord a) => [a] -> [a] -- Función para quitar duplicados en una lista

quitaDupli [] = [] 
quitaDupli (x:xs) = if elem x xs 
                              then quitaDupli xs
                              else x : quitaDupli xs

dominio :: (Ord a) => Rel a -> [a]

dominio (R r) = quitaDupli (map (\f -> fst f) r)

-- soporte r = conjunto sobre el que esta definida la relacion r.

soporte :: (Ord a) => Rel a -> [a]

soporte (R r) = quitaDupli ((map (\f -> fst f) r) ++ (map (\f -> snd f) r))

-- relEquivalencia r = True si r es una relacion de equivalencia; False en caso contrario.

relEquivalencia :: (Ord a) => Rel a -> Bool

relEquivalencia (R r) = foldl (&&) True (map (\f -> (elem (fst f, fst f) r) && (elem (snd f, snd f) r) && (elem (snd f, fst f) r)) r)

-- conjCociente r = conjunto cociente de la relacion r, si esta es relación de equivalencia. Escribe un mensaje de error, en caso contrario.

conjAuxCoc :: Eq a => a -> [(a, a)] -> [a]

conjAuxCoc n r = map (\f -> snd f) (filter (\a -> fst a == n) r) -- Devuelve el conjunto cociente al que pertenece cada elemento de la relación

conjCociente :: (Ord a) => Rel a -> [[a]]

conjCociente (R r) = if relEquivalencia (R r)
                            then (map (\a -> (conjAuxCoc a r)) sop)
                            else error "La relación r no es una relación de equivalencia"
                            where sop = soporte (R r)

--generaDiv n m = r donde r es la relacion {(x, y) | n ≤ x, y ≤ m, x es divisor de y}.

generaAuxDiv :: (Integral a) => a -> a -> [(a, a)]

generaAuxDiv n m = (map (\g -> (g,m)) (filter (\f -> m `mod` f == 0) [1..n])) -- Coge los números hasta n que dividan al número m por el que vaya


generaDiv :: (Integral a) => a -> a -> Rel a

generaDiv n m = R(foldl (++) [] (map (\f -> generaAuxDiv n f) [1..m]))


-- generaGE xs = r donde r es la relacion ≥ sobre el conjunto de elementos de la lista xs.

generaAuxGE :: (Ord a) => a -> [a] -> [(a, a)]

generaAuxGE y [] = []
generaAuxGE y (x:xs) = if y >= x then [(y,x)] ++ generaAuxGE y xs -- Coge los que sean mayores que y
                       else generaAuxGE y xs

generaGE :: (Ord a) => [a] -> Rel a

generaGE [] = R([])
generaGE xs = R(foldl (++) [] (map (\f -> generaAuxGE f xs) xs))

-- composicion r1 r2 = r1 ◦ r2. Observa que las relaciones r1 y r2 tienen que estar definidas sobre el mismo conjunto.

compararSop :: (Ord a) => [a] -> [a] -> Bool

compararSop xs ys = (length xs == length ys) && (foldl (&&) True (map (\f -> (elem f ys)) xs)) -- Compara que dos soportes sean iguales

compAux :: (Ord a) => [(a,a)] -> [(a,a)] -> [(a,a)]

compAux [] rb = [] -- Crea la lista que crea la relación de la composición
compAux ra [] = []
compAux (x:ra) rb = (map (\f -> (fst x, snd f)) (filter (\f -> snd x == fst f) rb)) ++ compAux ra rb

composicion :: Ord a => Rel a -> Rel a -> Rel a

composicion  (R ra) (R rb) = if (compararSop (soporte (R ra)) (soporte (R rb))) then R(compAux ra rb)
                                                                 else error "Las dos relaciones deben tener el mismo soporte"

-- 4.

-- introRel lee una relación introducida por el usuario. Debe pedir al usuario que introduzca los pares de la relación uno a uno.

introRel :: IO ()

introRel = do
   putStrLn "Introduce un par:  (0 para salir)"
   sx <- getLine
   x <- return (read sx::Int)
   if x == 0 then return ()
   else do
    sy <- getLine
    y <- return (read sy::Int)
    print (R [(x,y)])
    introRel2 [(x,y)]

introRel2 :: [(Int, Int)] -> IO ()

introRel2 xs = do -- Lista por parámetro para ir almacenando las tuplas guardadas
   putStrLn "Introduce un par:  (0 para salir)"
   sx <- getLine
   x <- return (read sx::Int)
   if x == 0 then return ()
   else do
    sy <- getLine
    y <- return (read sy::Int)
    print (R (xs ++ [(x,y)]))
    introRel2 (xs ++ [(x,y)])






