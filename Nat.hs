module Nat where
import Prelude hiding (min,max,add)

data Nat = O | S Nat
    deriving ( Eq , Show )

min :: Nat -> Nat -> Nat
min O _ = O
min n O = O
min (S n) (S m) = S (min n m)

max :: Nat -> Nat -> Nat
max n O = n
max O n = n
max (S n) (S m) = S (max n m)

add :: Nat -> Nat -> Nat
add n O = n 
add O n = n 
add n (S m) = S (add n m)
