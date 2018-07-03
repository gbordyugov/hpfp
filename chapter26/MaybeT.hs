data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }


instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ fmap (fmap f) ma


instance (Applicative m) => Applicative (MaybeT m) where
  pure a = MaybeT $ pure $ pure a
  (MaybeT fab) <*> (MaybeT a) = MaybeT $ undefined -- TODO
