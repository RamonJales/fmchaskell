module Nat where
import Prelude hiding (sum, mult, exp, quot, min, gcd, lcm, div, max, pred, rem)

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
