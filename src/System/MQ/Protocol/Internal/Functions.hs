{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module System.MQ.Protocol.Internal.Functions
  (
    mkId
  , emptyId
  , notExpires
  , jsonEncoding
  , msgpackEncoding
  , createMessage
  , createMessageBS
  , getTimeMillis
  ) where

import           Control.Monad                     (when)
import           Control.Monad.Except              (throwError)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Data.ByteString                   (ByteString)
import           Data.Text                         as T (Text, append, pack)
import           System.Clock                      (Clock (..), getTime,
                                                    toNanoSecs)
import           System.MQ.Error.Internal.Types    (MQError (..), errorEncoding)
import           System.MQ.Monad                   (MQMonadS)
import           System.MQ.Protocol.Class          as MQClass (MessageLike (..),
                                                               Props (..))
import           System.MQ.Protocol.Internal.Types (Creator, Encoding, Id,
                                                    Message (..), MessageType,
                                                    Spec, Timestamp)
import           System.Random                     (randomRIO)

-- | Creates 'Hash' with empty content.
--
emptyId :: Id
emptyId = ""

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
createMessage :: forall a s. MessageLike a
              => Id        -- ^ parent message id
              -> Creator   -- ^ message creator id
              -> Timestamp -- ^ message expiration time
              -> a         -- ^ message data
              -> MQMonadS s Message
createMessage mPid mCreator mExpires (MQClass.pack -> mData) = do
    let Props{..}   = props :: Props a
    createMessageBS mPid mCreator mExpires spec encoding mtype mData

-- | Creates new message using already encoded data.
--
createMessageBS :: Id            -- ^ parent message id
                -> Creator       -- ^ message creator id
                -> Timestamp     -- ^ message expiration time
                -> Spec          -- ^ spec of message
                -> Encoding      -- ^ encoding of data
                -> MessageType   -- ^ type of data
                -> ByteString -- ^ message data
                -> MQMonadS s Message
createMessageBS mPid mCreator mExpires spec' encoding' mtype' mData = do
    when (encoding' /= jsonEncoding && encoding' /= msgpackEncoding) $ throwError encodingEr
    (mId, mCreated) <- mkId
    pure $ Message mId mPid mCreator mCreated mExpires spec' encoding' mtype' mData
  where
    encodingEr = MQError errorEncoding "unknown encoding"

-- | Get current time in milliseconds.
--
getTimeMillis :: MonadIO m => m Timestamp
getTimeMillis = (`div` 10^(6::Int)) <$> getTimeNano

-- | Creates 'Id' for the message.
-- First part is current time in milliseconds, second part is random 6 digits.
--
mkId :: MonadIO m => m (Id, Timestamp)
mkId = do
    created      <- getTimeMillis
    randomSuffix <- random6Digits
    let id'      = (T.pack . show $ created) `T.append` randomSuffix
    pure (id', created)

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

getTimeNano :: MonadIO m => m Timestamp
getTimeNano = liftIO $ fromIntegral . toNanoSecs <$> getTime Realtime

random6Digits :: MonadIO m => m Text
random6Digits = liftIO $ T.pack . drop 1 . show . (1000000 +) <$> randomRIO (0::Int, 999999)
