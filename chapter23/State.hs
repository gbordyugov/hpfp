data State s a = State { runState :: s -> (a, s) }


instance Functor (State s) where
  fmap f (State t) = State $ \s -> let (a, s') = t s in (f a, s')
