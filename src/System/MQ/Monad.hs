{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.MQ.Monad
  (
    MQMonad
  , runMQMonad
  , errorHandler
  , foreverSafe
  ) where

import           Control.Monad.Except           (ExceptT, MonadError, MonadIO,
                                                 catchError, forever, liftIO,
                                                 runExceptT)
import           System.Log.Logger              (errorM)
import           System.MQ.Error.Internal.Types (MQError (..))
import           Text.Printf                    (printf)


-- | 'MQMonad' is the base monad for the Monique System.
--
newtype MQMonad a = MQMonad { unMQMonad :: ExceptT MQError IO a }
  deriving ( Monad
           , Functor
           , Applicative
           , MonadIO
           , MonadError MQError
           )

-- | Turns 'MQMonad' into 'IO' monad.
-- If exception happens error will be thrown.
--
runMQMonad :: MQMonad a -> IO a
runMQMonad m = either renderError pure =<< runExceptT (unMQMonad m)
  where
    renderError :: MQError -> IO a
    renderError mqError = error . show $! mqError

-- | 'errorHandler' logs message with error @err@ for the component with name @name@
--
errorHandler :: String -> MQError -> MQMonad ()
errorHandler name (MQError c m) = do
  liftIO . errorM name $! printf "MQError (code %d): %s" c m
  pure ()

-- | 'foreverSafe' runs given @MQMonad ()@ forever.
-- If exception happens it prints log and runs further.
--
foreverSafe :: String -> MQMonad () -> MQMonad ()
foreverSafe name = forever . (`catchError` errorHandler name)
