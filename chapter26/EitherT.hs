data EitherT e m a = EitherT { runEitherT :: Either e (m a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ fmap (fmap f) ema
{-
instance (Monad m) => Monad (EitherT e m) where
  return a = EitherT $ return $ return a
  (EitherT ema) >>= f = undefined
-}
