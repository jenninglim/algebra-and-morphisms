> import Fix

> -- | Expr as an Algebraic Data Type.
> data Expr a = Val a
>            | Add (Expr a) (Expr a)
>            | Mul (Expr a) (Expr a)

> -- | Pattern Functor of Expr
> data ExprF a k = Val' a
>                | Add' k k
>                | Mul' k k

> -- | Functor Instance
> instance Functor (ExprF a) where
>   fmap f (Val' x)   = Val' x
>   fmap f (Add' x y) = Add' (f x) (f y)
>   fmap f (Mul' x y) = Mul' (f x) (f y)

> -- | Algebra
> eval :: (ExprF Int) Int -> Int
> eval (Val' x)   = x
> eval (Mul' x y) = x * y
> eval (Add' x y) = x + y

> -- | Recursive definition of eval.
> eval' :: Expr Int -> Int
> eval' (Val x)   = x
> eval' (Add x y) = (eval' x) + (eval' y)
> eval' (Mul x y) = (eval' x) * (eval' y)
