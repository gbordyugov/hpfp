data Writer w a = Writer { runWriter :: (a, w) }


instance Functor (Writer w) where
  fmap f (Writer aw) = Writer $ (f (fst aw), snd aw)


instance (Monoid w) => Applicative (Writer w) where
  pure x = Writer $ (x, mempty)
  (Writer f) <*> (Writer a) =
    Writer $ ((fst f) (fst a), (snd f) `mappend` (snd a))
