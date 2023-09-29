module Bool where
import Nat
import Prelude hiding (if_then_else, leq, (==), False, True, Bool, ev, od, isMul3)

data Bool = False | True
    deriving ( Eq , Show )

if_then_else :: Bool -> Nat -> Nat -> Nat
if_then_else True n _ = n
if_then_else False _ n = n

leq ::  Nat -> Nat -> Bool
leq O _ = True
leq _ O = False
leq (S n) (S m) = leq n m

(==) :: Nat-> Nat -> Bool
O == O = True
(S n) == (S m) = n == m
_ == _ = False

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
