module Nat where
import Prelude hiding (min,max,sum,mul,exp,doub,fat,fib,pred,if_then_else_,minus,div,
    leq,ev,od,isMul3,isZero)

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

sum :: Nat -> Nat -> Nat
sum n O = n 
sum O n = n 
sum n (S m) = S (sum n m)

mul :: Nat -> Nat -> Nat
mul n O = O
mul n (S O) = n 
mul n (S m) = (sum n (mul n m))

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
fib (S (S n)) = sum (fib (S n)) (fib n)

pred :: Nat -> Nat
pred O = O
pred (S n) = n

minus :: Nat -> Nat -> Nat
minus n O = n 
minus n (S m) = pred (minus n m) 

div :: Nat -> Nat -> (Nat , Nat)
div n m = if (leq n m) then (O, n) else (S n', m')
 where (n', m') = div (minus n m) m

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

od :: Nat -> Bool
od O = False
od (S O) = True
od (S (S n)) = od n

isMul3 :: Nat -> Bool 
isMul3 (S (S (S n))) = isMul3 n
isMul3 O = True 
isMul3 _ = False

isZero :: Nat -> Bool
isZero O = True
isZero _ = False 