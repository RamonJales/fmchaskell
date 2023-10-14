module ListNat where
import Prelude hiding(List)
import Nat

{-# LANGUAGE GADTs #-}

data List a where
    Empty :: List a
    Cons :: a -> List a -> List a
    deriving ( Eq , Show )
