data Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f (Writer aw) = Writer $ (f (fst aw), snd aw)
