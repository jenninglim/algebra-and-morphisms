--algebras!
type Algebra f a = f a -> a

--Fixpoint Combinator
data Fix f = In (f (Fix f))

inop :: Fix f -> f (Fix f)
inop (In f) = f

--recursive definition of Expr
data Expr a = Val a
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)

--nonrecursive defintion of Expr
data ExprF a k = Val' a
               | Add' k k
               | Mul' k k

--also a functor!
instance Functor (ExprF a) where
--  fmap :: (b -> c) -> ExprF a b -> ExprF a c
  fmap f (Val' x)   = Val' x
  fmap f (Add' x y) = Add' (f x) (f y)
  fmap f (Mul' x y) = Mul' (f x) (f y)

--catamorphism (a general fold?)
cata :: (Functor f) => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . inop

--some algebra...
eval :: ExprF Int Int -> Int
eval (Val' x)   = x
eval (Mul' x y) = x * y
eval (Add' x y) = x + y

--vs some rescursion.
eval' :: Expr Int -> Int
eval' (Val x)   = x
eval' (Add x y) = (eval' x) + (eval' y)
eval' (Mul x y) = (eval' x) * (eval' y)
