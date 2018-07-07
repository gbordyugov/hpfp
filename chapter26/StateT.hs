data StateT s m a = StateT { runStateT :: s -> m (a, s) }


instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT st) = StateT $ \s ->
    fmap (\(a, s) -> (f a, s)) (st s)


instance (Monad m) => Monad (StateT s m) where
  return a = StateT $ \s -> return (a, s)
  (StateT a) >>= f = StateT $ \s -> do
    (b, t) <- a s
    (runStateT (f b)) t

instance (Monad m) => Applicative (StateT s m) where
  pure = return
  (StateT f) <*> (StateT a) = StateT $ \s -> do
    (b, t) <- a s
    (g, q) <- f t
    return (g b, q)
