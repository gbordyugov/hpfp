data WriterT w m a = WriterT { runWriterT :: m (a, w) }
