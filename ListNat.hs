module ListNat where
import Prelude hiding()
import Nat

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )
