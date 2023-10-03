module ListNat where
import Prelude hiding(Nil, length, sum, product, elem, (++), Empty, reverse, rem, True, Bool, False)
import Nat
import Bool

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
elem n (Cons x xs) = if_then_else_2 (eq n x) True (elem n xs)

append :: Nat -> ListNat -> ListNat
append n Empty = Cons n Empty
append n (Cons x xs) = Cons x (append n xs)

reverse :: ListNat -> ListNat
reverse Empty = Empty
reverse (Cons x xs) = append x (reverse xs)

allEven :: ListNat -> Bool
allEven Empty = True
allEven (Cons x xs) = if_then_else_2 (ev x) (allEven xs) False

anyEven :: ListNat -> Bool
anyEven Empty = False
anyEven (Cons x xs) = if_then_else_2 (ev x) True (anyEven xs)

allOdd :: ListNat -> Bool
allOdd Empty = True
allOdd (Cons x xs) = if_then_else_2 (od x) (allOdd xs) False

anyOdd :: ListNat -> Bool
anyOdd Empty = False
anyOdd (Cons x xs) = if_then_else_2 (od x) True (anyOdd xs)

allZero :: ListNat -> Bool
allZero Empty = True
allZero (Cons x xs) = if_then_else_2 (eq x O) (allZero xs) False
