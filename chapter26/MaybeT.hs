data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }


instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ fmap (fmap f) ma


instance (Applicative m) => Applicative (MaybeT m) where
  pure a = MaybeT $ pure $ pure a
  (MaybeT fab) <*> (MaybeT a) = MaybeT $ (fmap (<*>) fab) <*> a


instance (Monad m) => Monad (MaybeT m) where
  return a = MaybeT $ return $ return a
  (MaybeT ma) >>= f = MaybeT $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      Just x  -> runMaybeT $ f x
