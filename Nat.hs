module Nat where
import Prelude hiding (sum, mult, exp, quot, min, gcd, lcm, div, max, pred, rem, minus, 
  if_then_else, leq, eq, ev, od, isMul3, divides)

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

div :: Nat -> Nat -> (Nat, Nat)
div _ O = error "Zero is not divisor!"
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

-- Bool

leq ::  Nat -> Nat -> Bool
leq O O = False
leq O _ = True
leq _ O = False
leq (S n) (S m) = leq n m

eq :: Nat-> Nat -> Bool
eq O O = True
eq (S n) (S m) = eq n m
eq _ _ = False

ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S(S n)) = ev n

od :: Nat -> Bool
od (S O) = True
od O = False
od (S(S n)) = od n

isMul3 :: Nat -> Bool
isMul3 (S(S(S n))) = isMul3 n
isMul3 O = True
isMul3 _ = False

natToBool :: Nat -> Bool
natToBool (S O) = True
natToBool O = False

divides :: Nat -> Nat -> Bool
divides n m = natToBool (if (eq (rem n m) O) then (S O) else O)

isZero :: Nat -> Bool
isZero O = True
isZero _ = False
