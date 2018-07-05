data State s a = State { runState :: s -> (a, s) }


instance Functor (State s) where
  fmap f (State t) = State $ \s -> let (a, s') = t s in (f a, s')


instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (State f) <*> (State a) = State $ \s ->
    let
      (h, t) = f s
      (b, q) = a t
    in
      (h b, q)


instance Monad (State s) where
  return = pure
  (State a) >>= f = State $ \s ->
    let
      (b, t) = a s
    in
      (runState $ f b) t
