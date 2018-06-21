{-# LANGUAGE ViewPatterns #-}

module System.MQ.Protocol.Class
  (
    MessageLike (..)
  , Props (..)
  ) where

import           Control.Monad.Except              (throwError)
import qualified Data.ByteString                   as BS (ByteString)
import qualified Data.ByteString.Lazy              as BSL (fromStrict, toStrict)
import qualified Data.MessagePack                  as MP (MessagePack (..),
                                                          pack, unpack)
import           System.MQ.Error.Internal.Types    (MQError (..), errorEncoding)
import           System.MQ.Monad                   (MQMonadS)
import           System.MQ.Protocol.Internal.Types (MessageType, Spec)
import           Text.Printf                       (printf)




-- | Every message in Monique system has fixed properties, that can be describe with type 'Props'.
--
data Props a = Props { spec  :: Spec        -- ^ message spec
                     , mtype :: MessageType -- ^ message type (Config, Result, Error or Data)
                     }

-- | 'MessageLike' connects meaningful information about message with it's properties.
-- "meaningful information" is 'ByteString' that is stored in message field @data@.
-- But content of the 'ByteString' is not enough to describe specification and type of the message.
-- So every message in haskell language should be instance of 'MessageLike'.
--
class MP.MessagePack a => MessageLike a where
  -- | Returns 'Props' for message.
  props :: Props a

  -- | Packs something to 'BS.ByteString'.
  pack :: a -> BS.ByteString
  pack = packMP

  -- | Unpacks something from 'BS.ByteString'.
  -- If 'unpack' failes then 'Nothing' will be returned.
  --
  unpack :: BS.ByteString -> Maybe a
  unpack = unpackMP

  -- | Unpacks something from 'BS.ByteString' inside 'MQMonad'.
  -- If 'unpackM' failes then 'MQError' will be thrown.
  --
  unpackM :: BS.ByteString -> MQMonadS s a
  unpackM = unpackMPMonad

-- | Packs something from 'MP.MessagePack' to 'BS.ByteString'.
--
packMP :: MP.MessagePack a => a -> BS.ByteString
packMP = BSL.toStrict . MP.pack

-- | Unpacks something from 'BS.ByteString' to 'MP.MessagePack'.
-- If 'unpack' failes then 'Nothing' will be returned.
--
unpackMP :: MP.MessagePack a => BS.ByteString -> Maybe a
unpackMP = MP.unpack . BSL.fromStrict

-- | Unpacks something from 'BS.ByteString' to 'MP.MessagePack' inside 'MQMonad'.
-- If 'unpackM' failes then 'MQError' will be thrown.
--
unpackMPMonad :: MP.MessagePack a => BS.ByteString -> MQMonadS s a
unpackMPMonad bs@(unpackMP -> m) = maybe (throwError err) pure m
  where
    err = MQError errorEncoding . printf "could not unpack MessagePack: %s" . show $ bs
