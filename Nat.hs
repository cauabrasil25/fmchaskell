module Nat where
import Prelude hiding (min,max,add,mul,exp,doub,fat,fib)

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

mul :: Nat -> Nat -> Nat
mul n O = O
mul n (S O) = n 
mul n (S m) = (add n (mul n m))

exp :: Nat -> Nat -> Nat
exp n O = (S O)
exp O n = O
exp n (S m) = mul n (exp n m)

doub :: Nat -> Nat
doub O = O
doub n = mul n n 

fact :: Nat -> Nat
fact O = (S O)
fact (S n) = mul (S n) (fact n)

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = add (fib (S n)) (fib n)