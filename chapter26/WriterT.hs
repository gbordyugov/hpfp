data WriterT w m a = WriterT { runWriterT :: m (a, w) }


instance (Functor m) => Functor (WriterT w m) where
  fmap f (WriterT maw) =
    WriterT $ fmap (\(a, w) -> (f a, w)) maw
