data ReaderT r m a = ReaderT { runReaderT :: r -> m a }


instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (\r -> fmap f (rma r))


{-
(WriterT f) <*> (WriterT a) = WriterT $ (fmap h f) <*> a where
  h (f, fw) = \(a, aw) -> (f a, fw `mappend` aw)
-}

instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = ReaderT $ \r -> pure x
  (ReaderT f) <*> (ReaderT a) = ReaderT $ undefined
