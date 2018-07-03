data EitherT e m a = EitherT { runEitherT :: Either e (m a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ fmap (fmap f) ema

instance (Applicative m) => Applicative (EitherT e m) where
  pure a = EitherT $ pure $ pure a
  (EitherT fa) <*> (EitherT a) = EitherT $ (fmap (<*>) fa) <*> a
{-
instance (Monad m) => Monad (EitherT e m) where
  return a = EitherT $ return $ return a
  (EitherT ema) >>= f = undefined
-}
