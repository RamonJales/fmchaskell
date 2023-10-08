module ListNat where
import Prelude hiding(length, sum, product, elem, (++), Empty, reverse, rem, exp, enumFromTo)
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
elem n (Cons x xs) = if eq n x then True else elem n xs

append :: Nat -> ListNat -> ListNat
append n Empty = Cons n Empty
append n (Cons x xs) = Cons x (append n xs)

reverse :: ListNat -> ListNat
reverse Empty = Empty
reverse (Cons x xs) = append x (reverse xs)

allEven :: ListNat -> Bool
allEven Empty = True
allEven (Cons x xs) = if ev x then allEven xs else  False

anyEven :: ListNat -> Bool
anyEven Empty = False
anyEven (Cons x xs) = if ev x then True else anyEven xs

allOdd :: ListNat -> Bool
allOdd Empty = True
allOdd (Cons x xs) = if od x then allOdd xs else False

anyOdd :: ListNat -> Bool
anyOdd Empty = False
anyOdd (Cons x xs) = if od x then True else anyOdd xs

allZero :: ListNat -> Bool
allZero Empty = True
allZero (Cons x xs) = if eq x O then allZero xs else False

anyZero :: ListNat -> Bool
anyZero Empty = False
anyZero (Cons x xs) = if eq x O then True else anyZero xs

addNat :: Nat -> ListNat -> ListNat
addNat _ Empty = Empty
addNat n (Cons x xs) = (Cons (sum n x) (addNat n xs))

multNat :: Nat -> ListNat -> ListNat
multNat _ Empty = Empty
multNat n (Cons x xs) = (Cons (mult n x) (multNat n xs))

expNat :: Nat -> ListNat -> ListNat
expNat _ Empty = Empty
expNat n (Cons x xs) = (Cons (exp n x) (expNat n xs))

enumFromTo :: Nat -> Nat -> ListNat
enumFromTo n m = if leq m n then Empty else Cons n (enumFromTo (sum n (S O)) m)
