module ListNat where
import Nat
import Bool
import Empty
import Prelude hiding (Nil, length)

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )


length :: ListNat -> Nat
length Empty = O
length (Cons x xs) = S(length xs)
