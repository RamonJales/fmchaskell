module ListNat where
import Nat
import Bool
import Empty
import Prelude hiding (Nil, length, sum)

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )


length :: ListNat -> Nat
length Empty = O
length (Cons x xs) = S(length xs)

sumList :: ListNat -> Nat
sumList Empty = O
sumList (Cons x xs) = sum x (sumList xs)