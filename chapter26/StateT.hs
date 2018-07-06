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

{-
what :: Applicative m =>
  StateT s m (a -> b) -> StateT s m a -> StateT s m b
what (StateT f) (StateT a) = StateT $ \s ->
  let
    mfs :: Applicative m => m (a -> b, s)
    mfs = f s
    tmp = f' <$> mfs
    f' :: (a -> b, s) -> (s -> (a, s)) -> (b, s)
    f' (f'', s) a =
      let
        (a', t) = a s
      in
        (f'' a', t)
  in
    tmp <*> a
-}

{-
what :: Applicative m => (s -> m (a -> b, s)) -> (s -> m (a, s)) -> (s -> m (b, s))
-}
what f a = \s ->
  let
    mfs = f s
    tmp = g <$> mfs
    g (f', s')
  in
    undefined


instance (Applicative m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure $ (a, s)
  (StateT f) <*> (StateT a) = undefined
  {- (StateT f) <*> (StateT a) = StateT $ \s ->
    let
      mfs = f s
      tmp = h <$> mfs
      h :: (a -> b, s) -> (s -> (a, s)) -> (b, s)
      h (f, s) a =
        let
          (a', t) = a s
        in
          (f a', t)
    in
      -- undefined
      tmp <*> a
      -}
