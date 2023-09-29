module Bool where
import Nat
import Prelude hiding (if_then_else)

data Bool = False | True
    deriving ( Eq , Show )

if_then_else :: Bool -> Nat -> Nat -> Nat
if_then_else True n _ = n
if_then_else False _ n = n
