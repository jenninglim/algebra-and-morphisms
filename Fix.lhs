FIX POINTS
==========

> module Fix where

Algebra

> type Algebra f a = f a -> a

Fix Points

> data Fix f = In { inop :: f (Fix f) }

Catamorphism

> cata :: Functor f => Algebra f a -> Fix f -> a
> cata alg = alg . fmap (cata alg) . inop
