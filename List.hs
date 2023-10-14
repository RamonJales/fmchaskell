module ListNat where
import Prelude hiding(List, map, filter)
import Nat

{-# LANGUAGE GADTs #-}

data List a where
    Empty :: List a
    Cons :: a -> List a -> List a
    deriving ( Eq , Show )

map :: (a -> b) -> List a -> List b
map f Empty = Empty
map f (Cons x xs) = Cons (f x) (map f xs)

append :: a -> List a -> List a
append n Empty = Cons n Empty
append n (Cons x xs) = Cons x (append n xs)