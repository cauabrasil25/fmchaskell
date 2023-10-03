module Nat where
import Prelude hiding (min,max,add,mul,exp,doub,fat,fib,pred,if_then_else_,leq,ev)

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

double :: Nat -> Nat
double O = O
double n = mul n n 

fact :: Nat -> Nat
fact O = (S O)
fact (S n) = mul (S n) (fact n)

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = add (fib (S n)) (fib n)

pred :: Nat -> Nat
pred O = O
pred (S n) = n

--bool
if_then_else_ :: Bool -> Nat -> Nat -> Nat 
if_then_else_ True n _ = n 
if_then_else_ False _ n = n 

leq :: Nat -> Nat -> Bool
leq O n = True
leq n O = False
leq (S n) (S m) = leq n m 

ev :: Nat -> Bool
ev O = True
ev (S O) = False 
ev (S (S n)) = ev n

