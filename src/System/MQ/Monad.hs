{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module System.MQ.Monad
  (
    MQMonad
  , MQError (..)
  , runMQMonad
  , errorHandler
  , foreverSafe
  ) where

import           Control.Monad.Except (ExceptT, MonadError, MonadIO, catchError,
                                       forever, liftIO, runExceptT)
import           System.Log.Logger    (errorM)
import           Text.Printf          (printf)

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
    renderError ms = ms `seq` print ms >> error "Shit happens"

-- | 'errorHandler' logs message with error @err@ for the component with name @name@
--
errorHandler :: (Show e) => String -> e -> MQMonad ()
errorHandler name err = do
  liftIO . errorM name . show $! err
  pure ()

-- | 'foreverSafe' runs given @MQMonad ()@ forever.
-- If exception happens it prints log and runs further.
--
foreverSafe :: String -> MQMonad () -> MQMonad ()
foreverSafe name = forever . (`catchError` errorHandler name)

-- | 'MQError' is class for MoniQue Errors.
--
data MQError
  = MQProtocolError  { msg :: String }
  | MQTransportError { msg :: String }
  | MQTechnicalError { msg :: String }
  | MQComponentError { msg :: String }
  deriving (Eq, Ord)

instance Show MQError where
  show MQProtocolError{..}  = printf "MoniQue protocol error: %s" msg
  show MQTransportError{..} = printf "Monique transport error: %s" msg
  show MQTechnicalError{..} = printf "MoniQue technical error: %s" msg
  show MQComponentError{..} = printf "MoniQue component runtime error: %s" msg
