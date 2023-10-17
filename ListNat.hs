module ListNat where
import Prelude hiding(length,sumlist,sum,mul,product)
import Nat

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq , Show )

length :: ListNat -> Nat 
length Empty = O 
length (Cons x xs) = (S (length (xs)))

sumlist :: ListNat -> Nat
sumlist Empty = O
sumlist (Cons x xs) = sum x (sumlist xs)

product :: ListNat -> Nat
product Empty = (S O)
product (Cons x xs) = mul x (product xs)