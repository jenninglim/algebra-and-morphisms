> {-# Language GADTs #-}
> {-# Language KindSignatures #-}
> {-# Language DeriveFunctor #-}

> import Fix

This will summarise the result from Folding Domain Specific Languages.

Consider the simple language of addition and booleans.

GADT attempt w/ Phantom Type?

> data Expr :: * -> * where
>   I   :: Int       -> Expr Int
>   B   :: Bool      -> Expr Bool
>   Add :: Expr Int  -> Expr Int -> Expr Int
>   And :: Expr Bool -> Expr Bool -> Expr Bool

> data ExprF r a :: * where
>   I'   :: Int         -> ExprF r Int
>   B'   :: Bool        -> ExprF r Bool
>   Add' :: r Int     -> r Int    -> ExprF r Int
>   And' :: r Bool    -> r Bool   -> ExprF r Bool

> eval :: Expr a -> a
> eval (I i) = i
> eval (B b) = b
> eval (Add e1 e2) = eval e1 + eval e2
> eval (And e1 e2) = eval e1 && eval e2

Fixpoints of GADT?
