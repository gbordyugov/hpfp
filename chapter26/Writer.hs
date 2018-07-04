data Writer w a = Writer { runWriter :: (a, w) }


instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer $ (f a, w)


instance (Monoid w) => Applicative (Writer w) where
  pure x = Writer $ (x, mempty)
  (Writer (f, fw)) <*> (Writer (a, aw)) =
    Writer $ (f a, fw `mappend` aw)


instance (Monoid w) => Monad (Writer w) where
  return = pure
  (Writer (a, w)) >>= f = let (Writer (a', w')) = f a
    in Writer $ (a', w `mappend` w')
