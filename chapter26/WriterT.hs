data WriterT w m a = WriterT { runWriterT :: m (a, w) }


instance (Functor m) => Functor (WriterT w m) where
  fmap f (WriterT maw) =
    WriterT $ fmap (\(a, w) -> (f a, w)) maw


{-
(Writer (f, fw)) <*> (Writer (a, aw)) =
  Writer $ (f a, fw `mappend` aw)
-}

instance (Applicative m, Monoid w) =>
                          Applicative (WriterT w m) where
  pure x = WriterT $ pure (x, mempty)
  (WriterT f) <*> (WriterT a) = WriterT $
    (fmap h f) <*> a where
      h = undefined
