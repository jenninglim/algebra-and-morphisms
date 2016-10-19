--algebras!
type Algebra f a = f a -> a

--Fixpoint Combinator
data Fix f = In (f (Fix f))

inop :: Fix f -> f (Fix f)
inop (In f) = f

--generalised fold.
cata :: (Functor f) => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . inop

type Size = Int

--Circuits as a Functor
--again k marks the recursive spot!
data CircuitF a k = IdentityF Size 
                   | FanF Size
                   | AboveF k k
                   | BesideF k k
                   | StretchF [Size] k

--as a functor
instance Functor (CircuitF a) where
  fmap f (IdentityF x)   = IdentityF x
  fmap f (FanF x)        = FanF x
  fmap f (AboveF x y)    = AboveF (f x) (f y)
  fmap f (BesideF x y)   = BesideF (f x) (f y)
  fmap f (StretchF xs y) = StretchF xs (f y)

--some algebra
width :: Algebra (CircuitF Int) Int
width (IdentityF x)   = x
width (FanF x)        = x
width (AboveF x _)    = x
width (BesideF x y)   = x + y
width (StretchF xs y) = sum xs
