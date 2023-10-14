module ListNat where
import Prelude hiding(List)
import Nat

{-# LANGUAGE GADTs #-}

data List a where
    Empty :: List a
    Cons :: a -> List a -> List a
    deriving ( Eq , Show )

append :: a -> List a -> List a
append n Empty = Cons n Empty
append n (Cons x xs) = Cons x (append n xs)