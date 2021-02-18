{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Chapter26 where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Maybe as TM
import qualified Control.Monad.Trans.Reader as TR
import qualified Control.Monad.Trans.State as TS
import Data.Bifunctor
import Data.Functor.Identity
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

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

embedded :: MaybeT (ExceptT String (TR.ReaderT () IO)) Int
embedded = MaybeT . ExceptT . TR.ReaderT . fmap return $ const (Right (Just 1)) -- return must be lifted first to create the IO context

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

-- 26.14 Chapter Exercises

-- Write the code

rDec :: Num a => TR.Reader a a
rDec = TR.ReaderT $ return . flip (-) 1

rShow :: Show a => TR.ReaderT a Identity String
rShow = TR.ReaderT $ return . show

sayHi :: (Show a, MonadIO m) => a -> m ()
sayHi = liftIO . putStrLn . (++) "Hi: " . show

rPrintAndInc :: (Num a, Show a) => TR.ReaderT a IO a
rPrintAndInc = TR.ReaderT $ \r -> do
  sayHi r
  return $ r + 1

sPrintIncAccum :: (Num a, Show a) => TS.StateT a IO String
sPrintIncAccum = TS.StateT $ \s -> do
  sayHi s
  return (show s, s + 1)

-- Fix the code

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: TM.MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- TM.runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)

-- Hit counter

data Config = Config
  { counts :: IORef (M.Map Text Integer),
    prefix :: Text
  }

type Scotty = ScottyT Text (TR.ReaderT Config IO)

type Handler = ActionT Text (TR.ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = let v = 1 + fromMaybe 0 (M.lookup k m) in (M.insert k v m, v)

app :: Scotty ()
app = get "/:key" $ do
  unprefixed <- param "key"
  config <- lift TR.ask
  let key' = mappend (prefix config) unprefixed
  c <- liftIO $ readIORef (counts config)
  let countsRef = counts config
      (newCounts, newInteger) = bumpBoomp key' c
  liftIO $ writeIORef countsRef newCounts
  html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show newInteger, "</h1>"]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR r = TR.runReaderT r config
  scottyT 3000 runR app
