module ListNat where
import Prelude hiding(length, sum, product, elem, (++), Empty, reverse, rem, exp, enumFromTo, drop, head, tail, init, last, take, minimum,
    min, maximum, max, Maybe, Nothing, Just)
import Nat
import Maybe

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

allSomething :: ListNat -> (Nat -> Bool) -> (ListNat -> Bool) -> Bool
allSomething Empty _ _ = True
allSomething (Cons x xs) p f = if p x then f xs else False

allEven :: ListNat -> Bool
allEven xs = allSomething xs ev allEven

anyEven :: ListNat -> Bool
anyEven Empty = False
anyEven (Cons x xs) = if ev x then True else anyEven xs

allOdd :: ListNat -> Bool
allOdd xs = allSomething xs od allOdd

anyOdd :: ListNat -> Bool
anyOdd Empty = False
anyOdd (Cons x xs) = if od x then True else anyOdd xs

allZero :: ListNat -> Bool
allZero xs = allSomething xs isZero allZero

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

enumTo :: Nat -> ListNat
enumTo n = enumFromTo O n

take :: Nat -> ListNat -> ListNat
take (S n) (Cons x xs) = Cons x (take n xs)
take _ _ = Empty

drop :: Nat -> ListNat -> ListNat
drop (S n) (Cons x xs) = Cons x (take n xs)

elemIndices :: Nat -> ListNat -> ListNat
elemIndices n xs = elemIndices' n xs O
    where
        elemIndices' :: Nat -> ListNat -> Nat -> ListNat
        elemIndices' _ Empty _ = Empty
        elemIndices' n (Cons x xs) k = if eq n x then Cons k (elemIndices' n xs (S k)) else elemIndices' n xs (S k)

pwAdd :: ListNat -> ListNat -> ListNat 
pwAdd (Cons x xs) (Cons y ys) = Cons (sum x y) (pwAdd xs ys)
pwAdd _ _ = Empty

pwMult :: ListNat -> ListNat -> ListNat
pwMult (Cons x xs) (Cons y ys) = Cons (mult x y) (pwMult xs ys)
pwMult _ _ = Empty

isSorted :: ListNat -> Bool
isSorted (Cons x Empty) = True
isSorted (Cons x (Cons y l)) = if leq x y then isSorted (Cons y l) else False

filterEven :: ListNat -> ListNat
filterEven Empty = Empty
filterEven (Cons x xs) = if ev x then Cons x (filterEven xs) else filterEven xs

filterOdd :: ListNat -> ListNat
filterOdd Empty = Empty
filterOdd (Cons x xs) = if od x then Cons x (filterOdd xs) else filterOdd xs

head :: ListNat -> Nat
head Empty = error "Empty List has not a head"
head (Cons x _) = x

safeHead :: ListNat -> Maybe Nat
safeHead Empty = Nothing
safeHead (cons x _) = Just x

tail :: ListNat -> ListNat
tail Empty = error "Empty List has not a tail"
tail (Cons _ xs) = xs

safeTail :: ListNat -> Maybe Nat
safeTail Empty = Nothing
safeTail (Cons _ xs) = Maybe xs 

init :: ListNat -> ListNat
init Empty = error "Empty List has not a init"
init (Cons x Empty) = Empty
init (Cons x xs) = Cons x (init xs)

last :: ListNat -> Nat
last Empty = error "Empty List has not a last"
last (Cons x Empty) = x
last (Cons _ xs) = last xs

minimum :: ListNat -> Nat
minimum Empty = error "Empty List has not a minimum"
minimum (Cons x Empty) = x
minimum (Cons x xs) = min x (minimum xs)

maximum :: ListNat -> Nat
maximum Empty = error "Empty List has not a maximum"
maximum (Cons x Empty) = x
maximum (Cons x xs) = max x (maximum xs)

isPrefixOf :: ListNat -> ListNat -> Bool
isPrefixOf Empty _ = True
isPrefixOf (Cons x xs) (Cons y ys) = if eq x y then isPrefixOf xs ys else False

mix :: ListNat -> ListNat -> ListNat
mix Empty xs = xs
mix xs Empty = xs
mix (Cons x xs) (Cons y ys) = Cons x (Cons y (mix xs ys))

intersperse :: Nat -> ListNat -> ListNat
intersperse _ Empty = Empty
intersperse n (Cons x xs) = Cons x (Cons n (intersperse n xs))
