import qualified Prelude (fmap)

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



data IdentityT m a = IdentityT { runIdentityT :: m a }


{-
 - instances
 -}

instance (MyFunctor m) => MyFunctor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT (fmap f ma)

instance (MyApplicative m) => MyApplicative (IdentityT m) where
  pure a = IdentityT (pure a)
  (IdentityT f) <*> (IdentityT a) = IdentityT (f <*> a)

{-
instance (MyMonad m) => MyMonad (IdentityT m) where
  return a = IdentityT (return a)
  (IdentityT ma) >>= f =
    IdentityT (ma >>= runIdentityT . f)
-}
