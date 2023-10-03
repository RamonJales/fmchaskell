module ListNat where
import Prelude hiding(Nil, length, sum, product, elem, (++), Empty)
import Nat

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )

length :: ListNat -> Nat
length Empty = O
length (Cons x xs) = S(length xs)

sumList :: ListNat -> Nat
sumList Empty = O
sumList (Cons x xs) = sum x (sumList xs)

product :: ListNat -> Nat
product Empty = (S O)
product (Cons x xs) = mult x (product xs)

binaryConcat :: ListNat -> ListNat -> ListNat
binaryConcat Empty xs =  xs
binaryConcat (Cons x xs) ys = Cons x (binaryConcat xs ys)

elem :: Nat -> ListNat -> Bool
elem _ Empty = False
elem n (Cons x xs) = if (n == x) then True else (elem n xs)

append :: Nat -> ListNat -> ListNat
append n Empty = Cons n Empty
append n (Cons x xs) = Cons x (append n xs)