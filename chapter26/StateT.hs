data StateT s m a = StateT { runStateT :: s -> m (a, s) }


instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT st) = StateT $ \s ->
    fmap (\(a, s) -> (f a, s)) (st s)

tmp :: (s -> (a -> b, s)) -> (s -> (a, s)) -> (s -> (b, s))
tmp f a = \s ->
  let
    (g, t) = f s
    (b, q) = a t
  in
    (g b, q)

instance (Applicative m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure $ (a, s)
  (StateT f) <*> (StateT a) = StateT $ \s ->
    let
      fs = f s
      -- g  = fmap (\(f, s) -> f
    in
      undefined
