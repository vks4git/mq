{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module System.MQ.Protocol.Internal.Functions
  (
    mkId
  , emptyId
  , notExpires
  , encrypted
  , notEncrypted
  , emptySignature
  , createMessage
  , createMessageBS
  , getTimeMillis
  ) where

import           Control.Monad                     (replicateM, when)
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
import           System.MQ.Protocol.Internal.Types (Creator, Id,Encrypted,Signature,
                                                    Message (..), MessageType,
                                                    Spec, Timestamp)
import           System.Random                     (getStdRandom, randomR)

-- | Creates 'Id' with empty content.
--
emptyId :: Id
emptyId = ""

-- | Variable to mark that encription is used.
--
encrypted :: Encrypted
encrypted = True

-- | Variable to mark that encription is not used.
--
notEncrypted :: Encrypted
notEncrypted = False

-- | Creates 'Signature' with empty content.
--
emptySignature :: Signature
emptySignature = ""

-- | If message has no expiration time then this function can be used.
--
notExpires :: Timestamp
notExpires = 0

-- | Creates new message.
--
createMessage :: forall a s. MessageLike a
              => Id        -- ^ parent message id
              -> Creator   -- ^ creator id
              -> Timestamp -- ^ expiration time
              -> Encrypted -- ^ encryption flag
              -> Signature -- ^ signature
              -> a         -- ^ message data
              -> MQMonadS s Message
createMessage mPid mCreator mExpires mEncrypted mSign (MQClass.pack -> mData) = do
    let Props{..}   = props :: Props a
    createMessageBS mPid mCreator mExpires spec mtype mEncrypted mSign mData

-- | Creates new message using already encoded data.
--
createMessageBS :: Id          -- ^ parent message id
                -> Creator     -- ^ message creator id
                -> Timestamp   -- ^ message expiration time
                -> Spec        -- ^ spec of message
                -> MessageType -- ^ type of data
                -> Encrypted   -- ^ encryption flag
                -> Signature   -- ^ signature
                -> ByteString  -- ^ message data
                -> MQMonadS s Message
createMessageBS mPid mCreator mExpires spec' mtype' enc' sign' mData = do
    mCreated <- getTimeMillis
    mId <- mkId
    pure $ Message mId mPid mCreator mCreated mExpires spec' mtype' mData enc' sign'
  where
    encodingEr = MQError errorEncoding "unknown encoding"

-- | Get current time in milliseconds.
--
getTimeMillis :: MonadIO m => m Timestamp
getTimeMillis = (`div` 10^(6::Int)) <$> getTimeNano

-- | Creates 'Id' for the message.
-- First part is current time in milliseconds, second part is random 6 digits.
--
mkId :: MonadIO m => m Id
mkId = do
    -- generator <- liftIO $ getStdGen
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
