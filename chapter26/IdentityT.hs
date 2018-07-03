data IdentityT m a = IdentityT { runIdentityT :: m a }


instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT $ fmap f ma


instance (Applicative m) => Applicative (IdentityT m) where
  pure a = IdentityT (pure a)
  (IdentityT f) <*> (IdentityT a) = IdentityT (f <*> a)


instance (Monad m) => Monad (IdentityT m) where
  return a = IdentityT (return a)
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= (runIdentityT . f)
