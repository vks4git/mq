{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Transport.Internal.Types
  (
    Host
  , Port
  , HostPort (..)
  , PushChannel
  , PullChannel
  , PubChannel
  , SubChannel
  , ConnectTo (..)
  , BindTo (..)
  , Subscribe (..)
  , System.ZMQ4.Context
  , anyHost
  , allTopics
  , closeM
  , contextM
  , localHost
  , showTCP
  , terminateM
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import           System.MQ.Monad        (MQMonadS)
import           System.MQ.Protocol     (MessageType, Spec)
import           System.ZMQ4            (Context, Pub, Pull, Push, Socket, Sub,
                                         close, context, term)
import           Text.Printf            (printf)

-- | Alias for host
--
type Host = String

-- | Alias for port
--
type Port = Int

-- | Container for 'Host' and 'Port'
--
data HostPort = HostPort { host :: Host
                         , port :: Port
                         }

-- | Channel to 'Push' messages
--
type PushChannel = Socket Push

-- | Channel to 'Pull' messages
--
type PullChannel = Socket Pull

-- | Channel to 'Pub' messages
--
type PubChannel  = Socket Pub

-- | Channel to 'Sub' messages
--
type SubChannel  = Socket Sub

-- | Class for connecting to the given 'HostPort'.
--
class ConnectTo a where
  connectTo :: MonadIO m => HostPort -> Context -> m a

-- | Class for bind to the given 'HostPort'.
--
class BindTo a where
  bindTo :: MonadIO m => HostPort -> Context -> m a

-- | Class to subscribe and unsubscribe channel to topics
--
class Subscribe a where
  subscribeTo :: MonadIO m => a -> Text -> m ()

  subscribeToTypeSpec :: MonadIO m => a -> MessageType -> Spec -> m ()

  unsubscribeFrom :: MonadIO m => a -> Text -> m ()
  
  unsubscribeFromTypeSpec :: MonadIO m => a -> MessageType -> Spec -> m ()

-- | Sometimes wildcald host is needed for connections
--
anyHost :: Host
anyHost = "*"

-- | Describes any topic to subscribe, like a wildcard
--
allTopics :: Text
allTopics = ""

-- | Alias for localhost
--
localHost :: Host
localHost = "127.0.0.1"

-- | Converts 'Host' and 'Port' into tcp adress
--
showTCP :: Host -> Port -> String
showTCP = printf "tcp://%s:%d"

-- | Returns 'Context' in 'MQMMonad' inspite of 'IO' monad.
--
contextM :: MonadIO m => m Context
contextM = liftIO context

closeM :: Socket a -> MQMonadS s ()
closeM = liftIO . close

-- | Terminates given context in 'MQMonad'.
--
terminateM :: Context -> MQMonadS s ()
terminateM = liftIO . term
