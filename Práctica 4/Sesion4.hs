-- Práctica 4 - Álvaro Corrochano López

-- Ejercicio 1

data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Enum, Eq, Ord, Show)

mov :: Num a => [a] -> Direccion -> [a]

mov xs Arriba =  head xs : (last xs + 1) : []
mov xs Abajo =  head xs : (last xs - 1) : []
mov xs Derecha =  (head xs + 1): last xs : []
mov xs Izquierda =  (head xs - 1): last xs : []

destino :: Num a => [a] -> [Direccion] -> [a]

destino xs xm = foldl (mov) xs xm

-- Ejercicio 2

data Nat = Zero | Suc Nat deriving (Eq, Ord)

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x
  | x < 0 = error "Los numeros negativos no pueden convertirse en naturales"
  | otherwise = Suc (intToNat (x-1)) 

(.+) :: Nat -> Nat -> Nat

Zero .+ a = a
(Suc a) .+ b = Suc(a .+ b)


(.*) :: Nat -> Nat -> Nat
(.*) Zero a = Zero
(.*) (Suc a) b = (a .* b) .+ b

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Suc a) = 1 + natToInt a

instance Show Nat where
   show Zero = "0"
   show (Suc a) = show (1 + (natToInt a))
   
-- Ejercicio 3

data Comp = Complejo Float Float deriving (Eq)

instance Show Comp where
   show (Complejo a b) = (show a) ++ (show '+') ++(show b) ++ (show 'i')

instance Num Comp where
   (+) (Complejo a b) (Complejo c d) = Complejo (a+c) (b+d)
   (-) (Complejo a b) (Complejo c d) = Complejo (a-c) (b-d)
   (*) (Complejo a b) (Complejo c d) = Complejo ((a * c) - (b * d)) ((a * d) + (b * c))

-- Ejercicio 4

class Medible a where
      medida :: a -> Int

instance Medible Bool where
         medida False = 0
         medida True = 1

instance Medible [a] where
         medida [] = 0
         medida xs = 1 + (medida(tail xs)) -- Número de elemenos en la lista

instance Medible (a,b) where
         medida xs = 2

