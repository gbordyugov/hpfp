data StateT s m a = StateT { runStateT :: s -> m (a, s) }


instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT st) = StateT $
    \s -> fmap (\(a, s) -> (f a, s)) (st s)


instance (Applicative m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure $ (a, s)
  (StateT f) <*> (StateT a) = StateT undefined
