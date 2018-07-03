data Compose f g a = Compose { runCompose :: f (g a) }


instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap h (Compose fga) = Compose $ fmap (fmap h) fga


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . (pure . pure)
  (Compose f) <*> (Compose a) = Compose $ h <*> a where
    h = fmap (<*>) f
