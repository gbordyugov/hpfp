data Compose f g a = Compose { runCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap h (Compose fga) = Compose $ fmap (fmap h) fga
