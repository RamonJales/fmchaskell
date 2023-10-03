module Nat where
import Prelude hiding (sum, mult, exp, quot, min, gcd, lcm, div, max, pred, rem, minus)

data Nat = O | S Nat
    deriving (Eq , Show)

sum :: Nat -> Nat -> Nat
sum n O = n
sum n (S m) = S (sum n m)

mult :: Nat -> Nat -> Nat
mult n O = O
mult n (S m) = sum n (mult n m)

exp :: Nat -> Nat -> Nat
exp n O = (S O)
exp n (S m) = mult n (exp n m)

fact :: Nat -> Nat
fact O = (S O)
fact (S n) = mult (S n) (fact n)

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = sum (fib (S n)) (fib (n))

min :: Nat -> Nat -> Nat
min n O = O
min O n = O
min (S n) (S m) = S(min n m)

max :: Nat -> Nat -> Nat
max n O = n
max O n = n
max (S n) (S m) = S(max n m)

pred :: Nat -> Nat
pred O = O
pred (S n) = n

double :: Nat -> Nat
double O = O
double (S n) = (S(S(double n)))

minus :: Nat -> Nat -> Nat
minus n O = n
minus n (S m) = pred (minus n m)

-- Função usando o Bool do Haskell
leq ::  Nat -> Nat -> Bool
leq O _ = True
leq _ O = False
leq (S n) (S m) = leq n m

div :: Nat -> Nat -> (Nat, Nat)
div O _ = error "Zero is not divisor!"
div n m = if (leq n m) then (O, n) else (S n', m')
  where (n', m') = div (minus n m) m

quot :: Nat -> Nat -> Nat
quot n m = fst(div n m)

rem :: Nat -> Nat -> Nat
rem n m = snd(div n m)

-- Algoritmo de Euclides
gcd :: Nat -> Nat -> Nat
gcd n O = n;
gcd n m = gcd n (rem n m) 

-- [Discutido em salas de estudos e monitorias também]
lcm :: Nat -> Nat -> (Nat, Nat)
lcm n O = (O,O)
lcm n m = div (mult n m) (gcd n m)

-- --[Essa questão merece créditos, mas não dá para colocar um crédito em específico, pois muita gente discutiu sobre ela em monitorias e salas de estudo]
-- quot :: Nat -> Nat -> Nat
-- quot n m = quot' n m m
--   where
--     quot' :: Nat -> Nat -> Nat -> Nat
--     quot' O O k = S O
--     quot' O m k = O
--     quot' n O k = S (quot' n k k)
--     quot' (S n) (S m) k = quot' n m k

-- --[De novo, não tenho total crédito na criação dessa função]
-- rem :: Nat -> Nat -> Nat
-- rem O n = O
-- rem (S m) O = S (rem m O)
-- rem m n = rem' m (mult n (quot m n))
--   where
--     rem' :: Nat -> Nat -> Nat
--     rem' (S m) (S n) = rem' m n
--     rem' m O = m

-- div :: Nat -> Nat -> (Nat, Nat)
-- div n m = (quot n m, rem n m)
