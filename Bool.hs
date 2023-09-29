module Bool where
import Nat
import Prelude hiding (if_then_else)

data Bool = False | True
    deriving ( Eq , Show )
