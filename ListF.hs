--algebras!
type Algebra f a = f a -> a

--Fixpoint Combinator
data Fix f = In (f (Fix f))

inop :: Fix f -> f (Fix f)
inop (In f) = f

--k marks the spot!
data ListF a k = Nil
              | Cons a k

--listF as a functor...
instance Functor (ListF a) where
-- fmap :: (b -> c) -> ListF a b -> ListF a c
  fmap f Nil        = Nil
  fmap f (Cons a k) = Cons a $ f k

--generalised fold.
cata :: (Functor f) => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . inop

--some algebra...
length' :: Algebra (ListF Int) Int
length'  Nil        = 0
length' (Cons _ k) = 1 + k
