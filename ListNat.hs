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

(++) :: ListNat -> ListNat -> ListNat
xs ++ Empty = xs 
Empty ++ xs =  xs
(Cons x xs) ++ (Cons y ys) = Cons (sum x y) (xs ++ ys)

elem :: Nat -> ListNat -> Bool
elem _ Empty = False
elem n (Cons x xs) = if (n == x) then True else (elem n xs)