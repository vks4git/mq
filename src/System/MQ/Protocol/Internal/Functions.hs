{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.MQ.Protocol.Internal.Functions
  (
    emptyHash
  , notExpires
  , jsonEncoding
  , msgpackEncoding
  , createMessage
  , getTimeMillis
  ) where

import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Crypto.Hash.SHA1                  (hash)
import           Data.ByteString                   as BS (ByteString,
                                                          intercalate)
import           Data.ByteString.Base64            as Base64 (encode)
import           Data.String                       (IsString (..))
import           System.Clock                      (Clock (..), getTime,
                                                    toNanoSecs)
import           System.MQ.Protocol.Internal.Types (Creator, Encoding, Hash,
                                                    Message (..), Spec,
                                                    Timestamp)
import System.MQ.Protocol.Class (MessageLike (..), Props (..))

-- | Creates 'Hash' with empty content.
--
emptyHash :: Hash
emptyHash = ""

-- | If message has no expiration time then this function can be used.
--
notExpires :: Timestamp
notExpires = 0

-- | Alias for JSON encoding.
--
jsonEncoding :: Encoding
jsonEncoding = "JSON"

-- | Alias for MessagePack encoding.
--
msgpackEncoding :: Encoding
msgpackEncoding = "MessagePack"

-- | Creates new message.
--
createMessage :: forall m a. (MonadIO m, MessageLike a)
              => Hash      -- ^ parent message id
              -> Creator   -- ^ message creator id
              -> Timestamp -- ^ message expiration time
              -> a         -- ^ message data
              -> m Message 
createMessage mPid mCreator mExpires (pack -> mData) = do
    let Props{..}   = props :: Props a
    (mId, mCreated) <- mkId mCreator spec
    pure $ Message mId mPid mCreator mCreated mExpires spec encoding mtype mData

-- | Get current time in milliseconds.
--
getTimeMillis :: MonadIO m => m Timestamp
getTimeMillis = (`div` 10^(6::Int)) <$> getTimeNano

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

-- | Creates id 'Hash' and created time from 'Creator' and 'Spec'.
-- ATTENTION: identificator is in Base64 encoding.
--
mkId :: MonadIO m => Creator -> Spec -> m (Hash, Timestamp)
mkId mCreator mSpec = do
    mCreated <- getTimeMillis
    let mId = Base64.encode . hash . intercalate ":" $ [fromString mCreator, timestampToBS mCreated, fromString mSpec]
    pure (mId, mCreated)

getTimeNano :: MonadIO m => m Timestamp
getTimeNano = liftIO $ fromIntegral . toNanoSecs <$> getTime Realtime

timestampToBS :: Timestamp -> ByteString
timestampToBS = fromString . show
