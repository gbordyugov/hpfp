data StateT s m a = StateT { runStateT :: s -> m (a, s) }


instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT st) = StateT $ \s ->
    fmap (\(a, s) -> (f a, s)) (st s)

tmp1 :: (s -> (a -> b, s)) -> (s -> (a, s)) -> (s -> (b, s))
tmp1 f a = \s ->
  let
    (g, t) = f s
    (b, q) = a t
  in
    (g b, q)

tmp2 :: (a -> b, s) -> (a, s) -> (b, s)
tmp2 (fab, s) (a, t) = (fab a, t)

instance (Applicative m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure $ (a, s)
  (StateT f) <*> (StateT a) = StateT $ \s ->
    let
      mfs = f s
      tmp = fmap h mfs
      h :: (a -> b, s) -> (s -> (a, s)) -> (s -> (b, s))
      h (f, s) a =
        let
          (a', t) = a s
        in
          \s -> (f a', t)
    in
      tmp <*> a
