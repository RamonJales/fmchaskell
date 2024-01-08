module Int where
import Prelude hiding(Int, canon)
import Nat

{-# LANGUAGE GADTs #-}

data Int where
    I :: Nat -> Nat -> Int
    deriving(Eq, Show)

canon :: Int -> Int
canon (I O m) = I O m
canon (I n O) = I n O
canon (I (S n) (S m)) = canon (I n m)
