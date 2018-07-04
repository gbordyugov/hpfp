data ReaderT r m a = ReaderT { runReaderT :: r -> m a }


instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (\r -> fmap f (rma r))


instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = ReaderT $ \r -> pure x
  (ReaderT f) <*> (ReaderT a) = ReaderT $ \r -> (f r) <*> (a r)
