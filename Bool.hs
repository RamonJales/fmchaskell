module Bool where

import Prelude hiding (Bool, True, False)
import Nat

data Bool = False | True
    deriving ( Eq , Show )

if_then_else_2 :: Bool -> Bool -> Bool -> Bool
if_then_else_2 True n _ = n
if_then_else_2 False _ m = m

if_then_else :: Bool -> Nat -> Nat -> Nat
if_then_else True n _ = n
if_then_else False _ n = n