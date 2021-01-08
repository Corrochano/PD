-- Sesión 5 - Álvaro Corrochano López

--import System.IO

-- Ejercicio 1

adivina :: Int -> IO ()

adivina n =
    do putStrLn "Introduce un numero:"
       num <- getLine
       let x = read num::Int
       if x < n then do
            putStrLn $ "El numero " ++ show x ++ " es demasiado pequeño"
            adivina n
       else if x > n then do
            putStrLn $ "El numero " ++ show x ++ " es demasiado grande"
            adivina n
       else do putStrLn "Enhorabuena! Lo has adivinado!"

-- Ejercicio 2

formatea:: String -> String -> Int -> IO ()

--formatea fileIn fileOut n = do
--                             entrada <- readFile fileIn
--                             let part = words(entrada)
--                             let spaces = take (n - length(part)) (repeat ' ')
--                            let final = map (\x -> spaces ++ x) part
--                             writeFile fileOut (unwords(final))


formatea fileIn fileOut n = do
                             entrada <- readFile fileIn
                             let lineas = lines(entrada)
                             let part = map (\x -> words(x)) lineas
                             let spaces = take (n - length(part)) (repeat ' ')
                             let final = map (\x -> spaces ++ x) part
                             writeFile fileOut unlines((unwords(final)))



-- Ejercico 3

data Matriz = M [[Float]] deriving (Eq, Ord, Show)

-- a)

-- transp (M m) = 

sumaMat (M (m1)) (M(m2)) = map (\x -> zipWith (+) (fst x) (snd x)) $ zip m1 m2

-- b)

dibujaMatriz (M m) = dibujaMatrizAux m

dibujaMatrizAux [] = putStrLn("\n")
dibujaMatrizAux m = do
                     putStrLn $ show (head(m)) 
                     dibujaMatrizAux (tail(m))














