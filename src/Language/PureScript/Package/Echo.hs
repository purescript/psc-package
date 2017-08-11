-- | Tools for optionally `echo`ing 'Text' to stdout.
--
-- Use 'runSilentT' to silence 'echo'ed output in any 'MonadEcho'.

module Language.PureScript.Package.Echo (
  MonadEcho(..)
, SilentT(..)
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Text (Text)
import Data.Text.IO (putStrLn)
import Prelude hiding (putStrLn)

class Applicative m => MonadEcho m where
  echo :: Text -> m ()
instance MonadEcho IO where
  echo = putStrLn

-- | A `MonadEcho` that silences `echo`.
newtype SilentT m a = SilentT {runSilentT :: m a }

instance Functor m => Functor (SilentT m) where
    fmap f = SilentT . fmap f . runSilentT
    {-# INLINE fmap #-}

instance Applicative m => Applicative (SilentT m) where
    pure = SilentT . pure
    {-# INLINE pure #-}
    f <*> a = SilentT (runSilentT f <*> runSilentT a)
    {-# INLINE (<*>) #-}

instance Monad m => Monad (SilentT m) where
  return = SilentT . return
  SilentT ma >>= f = SilentT $ ma >>= runSilentT . f

instance MonadIO m => MonadIO (SilentT m) where
  liftIO = lift . liftIO

instance Applicative m => MonadEcho (SilentT m) where
  echo = const . SilentT $ pure ()

instance MonadTrans SilentT where
  lift = SilentT
