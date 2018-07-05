data StateT s m a = StateT { runStateT :: s -> m (a, s) }


instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT st) = StateT $
    \s -> fmap (\(a, s) -> (f a, s)) (st s)
