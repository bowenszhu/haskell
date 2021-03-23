data Nat = Zero | Suc Nat deriving (Show, Eq)

natToInteger :: Nat -> Integer
natToInteger Zero    = 0
natToInteger (Suc n) = 1 + natToInteger n

add :: Nat -> Nat -> Nat
add Zero    n = n
add (Suc m) n = Suc (add m n)

mult :: Nat -> Nat -> Nat
mult Zero    n = Zero
mult (Suc m) n = add n (mult m n)

pow :: Nat -> Nat -> Nat
pow m Zero    = Suc Zero
pow m (Suc n) = mult m (pow m n)

instance Ord Nat where
  -- (<=) :: Nat -> Nat -> Bool
  Zero  <= _     = True
  _     <= Zero  = False
  Suc x <= Suc y = x <= y

instance Num Nat where
  -- (+) :: Nat -> Nat -> Nat
  x + y = add x y

  -- (*) :: Nat -> Nat -> Nat
  x * y = mult x y

  -- fromInteger :: Integer -> Nat
  fromInteger 0 = Zero
  fromInteger x = Suc (fromInteger (x - 1))

x = Suc (Suc Zero)
y = Suc (Suc (Suc Zero))
