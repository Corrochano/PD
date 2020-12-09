-- Álvaro Corrrochano López DNI: 02711642B

-- Ejercicio 1

data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Enum, Eq, Ord, Show)

mov :: Num a => [a] -> Direccion -> [a]

mov xs Arriba =  head xs : (last xs + 1) : []
mov xs Abajo =  head xs : (last xs - 1) : []
mov xs Derecha =  (head xs + 1): last xs : []
mov xs Izquierda =  (head xs - 1): last xs : []

destino :: Num a => [a] -> [Direccion] -> [a]

destino xs xm = foldl (mov) xs xm

-- a)

trayectoria :: Num a => [a] -> [Direccion] -> [[a]]

trayectoria punto [] = []

trayectoria punto movs = (mov punto (head movs) ) : trayectoria (mov punto (head movs) ) (tail movs)

-- b)

--inferior :: Num a => [a] -> [Direccion] -> [Direccion] -> Bool

--inferior n movs movs' = 














