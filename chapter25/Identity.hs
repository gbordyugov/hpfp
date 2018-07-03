{-
 - Typeclasses definitions
 -}
class MyFunctor f where
  fmap :: (a -> b) -> f a -> f b 

class (MyFunctor f) => MyApplicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class (MyApplicative m) => MyMonad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b



-- Trivial data type
data Identity a = Identity { runIdentity :: a }


{-
 - instances
 -}

instance MyFunctor Identity where
  fmap f (Identity a) = Identity (f a)

instance MyApplicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance MyMonad Identity where
  return = Identity
  (Identity a) >>= f = f a
