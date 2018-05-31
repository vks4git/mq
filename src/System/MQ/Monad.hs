{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module System.MQ.Monad
  (
    MQMonad
  , MQMonadS
  , runMQMonad
  , runMQMonadS
  , evalMQMonadS
  , errorHandler
  , foreverSafe
  ) where

import           Control.Exception              (throw, Exception(..), SomeException)
import           Control.Monad.Except           (ExceptT, MonadError, MonadIO,
                                                 catchError, forever, liftIO,
                                                 runExceptT)
import           Control.Monad.State.Strict     (MonadState, StateT, runStateT)
import           System.Log.Logger              (errorM)
import           System.MQ.Error.Internal.Types (MQError (..))

-- | 'MQMonadS' is the base monad for the Monique System with state.
--
newtype MQMonadS s a = MQMonadS {unMQMonadS :: StateT s (ExceptT MQError IO) a }
  deriving ( Monad
           , Functor
           , Applicative
           , MonadIO
           , MonadError MQError
           , MonadState s)



-- | 'MQMonad' is the base monad for the Monique System.
--
type MQMonad a = MQMonadS () a

-- | Turns 'MQMonadS s a' into 'IO' monad.
-- If exception happens error will be thrown.
--
evalMQMonadS :: MQMonadS s a -> s -> IO a
evalMQMonadS m = fmap fst . runMQMonadS m

-- | Turns 'MQMonadS s a' into 'IO' monad with final state.
-- If exception happens error will be thrown.
--
runMQMonadS :: MQMonadS s a -> s -> IO (a, s)
runMQMonadS m state = either (throw . (toException::MQError -> SomeException)) pure =<< runExceptT (runStateT (unMQMonadS m) state)

-- | Turns 'MQMonad' into 'IO' monad.
-- If exception happens error will be thrown.
--
runMQMonad :: MQMonad a -> IO a
runMQMonad m = fst <$> runMQMonadS m ()

-- | 'errorHandler' logs message with error @err@ for the component with name @name@
--
errorHandler :: String -> MQError -> MQMonadS s ()
errorHandler name e = do
  liftIO . errorM name $! show e
  pure ()


-- | 'foreverSafe' runs given @MQMonadS s ()@ forever.
-- If exception happens it prints log and runs further.
--
foreverSafe :: String -> MQMonadS s () -> MQMonadS s ()
foreverSafe name = forever . (`catchError` errorHandler name)
