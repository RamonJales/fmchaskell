module Bool where
import Nat
import Prelude hiding (if_then_else, leq, (==), False, True, Bool)

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


-- ev :: Nat -> Bool