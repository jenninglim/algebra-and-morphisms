> import Fix

> data Nat k = Zero
>            | Succ k
>            deriving (Show)

> instance Functor Nat where
> -- | fmap :: (a -> b) -> f a -> f b
>   fmap f Zero     = Zero
>   fmap f (Succ k) = Succ (f k)

> toInt :: Fix Nat -> Int
> toInt = cata alg
>   where alg :: Nat Int -> Int
>         alg (Zero)   = 0
>         alg (Succ k) = k + 1

> fromInt :: Int -> Fix Nat
> fromInt 0 = In $ Zero
> fromInt x = In $ Succ $ fromInt (x-1)

> power :: Fix Nat -> Int
> power = cata alg
>   where alg :: Nat Int -> Int
>         alg (Zero)   = 1
>         alg (Succ x) = x * 2

> double :: Fix Nat -> Int
> double = cata alg
>  where alg :: Nat Int -> Int
>        alg (Zero)   = 0
>        alg (Succ x) = 2 + x

> power' :: Fix Nat -> Fix Nat
> power' = cata alg
>   where alg :: Nat (Fix Nat) -> (Fix Nat)
>         alg (Zero) = In (Succ (In Zero))
>         alg (Succ x) = double' x

> double' :: Fix Nat -> Fix Nat
> double' = cata alg
>   where alg :: Nat (Fix Nat) -> (Fix Nat)
>         alg (Zero) = In Zero
>         alg (Succ x) = In $ Succ $ In $ Succ x
