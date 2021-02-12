{-# LANGUAGE InstanceSigs #-}

module Chapter26 where

-- Exercises: EitherT

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . Right

  (EitherT mef) <*> (EitherT mea) = EitherT $ fmap (<*>) mef <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f = EitherT $ do
    ea <- mea
    case ea of
      (Left e) -> return $ Left e
      (Right a) -> runEitherT $ f a

swapEither :: Either a b -> Either b a
swapEither (Left l) = Right l
swapEither (Right r) = Left r

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT mab) = do
  ab <- mab
  case ab of
    (Left a) -> fa a
    (Right b) -> fb b
