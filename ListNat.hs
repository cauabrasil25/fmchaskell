module ListNat where
import Prelude hiding(length)
import Nat

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )

length :: ListNat -> Nat 
length Empty = O 
length (Cons x xs) = (S (length (xs)))