{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Chapter26 where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Reader as R
import Data.Bifunctor

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

-- Exercises: ReaderT

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ \r -> fmap f (rma r)

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))

  (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT (\r -> rma r >>= \a -> (runReaderT $ f a) r)

-- Exercises: StateT

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s -> fmap (first f) (sma s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT smab) <*> (StateT sma) = StateT $ \s -> do
    (fab, s1) <- smab s
    (a, s2) <- sma s1
    return (fab a, s2)

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (StateT sma) >>= f = StateT (sma >=> (\(a, s) -> (runStateT $ f a) s))

-- Exercise: Wrap It Up

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just

  (MaybeT mf) <*> (MaybeT ma) = MaybeT $ fmap (<*>) mf <*> ma

instance Monad m => Monad (MaybeT m) where
  return = pure

  (MaybeT ma) >>= f = MaybeT $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      (Just x) -> runMaybeT $ f x

embedded :: MaybeT (ExceptT String (R.ReaderT () IO)) Int
embedded = MaybeT . ExceptT . R.ReaderT . fmap return $ const (Right (Just 1)) -- return must be lifted first to create the IO context

-- Exercises: Lift More

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> fmap (,s) ma

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

-- Exercises: Some Instances

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
