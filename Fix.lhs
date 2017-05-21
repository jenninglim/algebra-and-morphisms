> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE UndecidableInstances #-}

FIX POINTS
==========

> module Fix where

Algebra
-------

> type Algebra f a = f a -> a

Fix Points
----------

This is where all the magic happens.

> data Fix f = In { inop :: f (Fix f) }
> deriving instance (Show (f (Fix f))) => Show (Fix f)
> -- | In   :: f (Fix f) -> Fix f
> -- | inop :: Fix f     -> f (Fix f)

Fix f is a type such that the functor will be arbitrarily
embedded into its type. This is the reason why pattern functors
mark its recursive parts with a parameter - the fix point will
be equivalent to its original definition.

inop can be thought as a peeking function that unwraps
the first layer of the fixpoint of functor.

Catamorphism
------------

> cata :: Functor f => Algebra f a -> Fix f -> a
> cata alg = alg . fmap (cata alg) . inop

The catamorphism is a generalised fold. The inop will peek
on the data type and then we will (this is the recurive part)
call the itself on the recursive parts of the data type - this
corresponds to fmap (cata alg). once it arrives at the base
case. The algebra will start transforming the data structure
by pattern matching on its structure until we have the desired result.

The similarity to folds now becomes obvious. It will traverse the data
structure (fmap (cata alg)) then collapse its structure step by step (alg)
until we have the desired result.
