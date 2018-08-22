{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module System.MQ.Protocol.Internal.Functions
  (
    emptyId
  , notExpires
  , jsonEncoding
  , mkId
  , msgpackEncoding
  , createMessage
  , createMessageBS
  , getTimeMillis
  ) where

import           Control.Monad                     (replicateM, when)
import           Control.Monad.Except              (throwError)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Data.ByteString                   as BS (ByteString)
import           Data.Text                         as T (pack)
import           System.Clock                      (Clock (..), getTime,
                                                    toNanoSecs)
import           System.MQ.Error.Internal.Types    (MQError (..), errorEncoding)
import           System.MQ.Monad                   (MQMonadS)
import           System.MQ.Protocol.Class          as MQClass (MessageLike (..),
                                                               Props (..))
import           System.MQ.Protocol.Internal.Types (Creator, Encoding, Id,
                                                    Message (..), MessageType,
                                                    Spec, Timestamp)
import           System.Random                     (getStdRandom, randomR)

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
    let Props{..} = props :: Props a
    createMessageBS mPid mCreator mExpires spec encoding mtype mData

-- | Creates new message using already encoded data.
--
createMessageBS :: Id            -- ^ parent message id
                -> Creator       -- ^ message creator id
                -> Timestamp     -- ^ message expiration time
                -> Spec          -- ^ spec of message
                -> Encoding      -- ^ encoding of data
                -> MessageType   -- ^ type of data
                -> BS.ByteString -- ^ message data
                -> MQMonadS s Message
createMessageBS mPid mCreator mExpires spec' encoding' mtype' mData = do
    when (encoding' /= jsonEncoding && encoding' /= msgpackEncoding) $ throwError encodingEr
    mCreated <- getTimeMillis
    mId <- mkId
    pure $ Message mId mPid mCreator mCreated mExpires spec' encoding' mtype' mData
  where
    encodingEr = MQError errorEncoding "unknown encoding"

-- | Get current time in milliseconds.
--
getTimeMillis :: MonadIO m => m Timestamp
getTimeMillis = (`div` 10^(6::Int)) <$> getTimeNano

-- | Creates 'Id' for the message.
--
mkId :: MonadIO m => m Id
mkId = do
    randomNumbers <- liftIO $ replicateM idLength $ getStdRandom (randomR (0, alphabetLength))
    pure . T.pack $ (alphabet !!) <$> randomNumbers
  where
    idLength :: Int
    idLength = 40



--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

alphabetLength :: Int
alphabetLength = length alphabet - 1

getTimeNano :: MonadIO m => m Timestamp
getTimeNano = liftIO $ fromIntegral . toNanoSecs <$> getTime Realtime
