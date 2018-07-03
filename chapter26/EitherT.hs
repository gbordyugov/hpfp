data EitherT e m a = EitherT { runEitherT :: m (Either e a) }


instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ fmap (fmap f) ema


instance (Applicative m) => Applicative (EitherT e m) where
  pure a = EitherT $ pure $ pure a
  (EitherT fa) <*> (EitherT a) = EitherT $ (fmap (<*>) fa) <*> a


instance (Monad m) => Monad (EitherT e m) where
  return a = EitherT $ return $ return a
  (EitherT ema) >>= f = EitherT $ do
    y <- ema
    case y of
      Left e  -> return $ Left e
      Right x -> runEitherT $ f x
