{-# LANGUAGE OverloadedStrings #-}


module System.MQ.Transport.ByteString
  (
    push
  , pull
  , pub
  , sub
  ) where

import           Control.Monad.Except
import qualified Data.ByteString                    as BS (ByteString, split)
import           Data.List.NonEmpty                 (NonEmpty (..))
import           System.MQ.Monad                    (MQError (..), MQMonad)
import           System.MQ.Protocol                 (delimiter)
import           System.MQ.Transport.Internal.Types (PubChannel, PullChannel,
                                                     PushChannel, SubChannel)
import           System.ZMQ4                        (receiveMulti, sendMulti)
import           Text.Printf                        (printf)

-- | Pushes @(tag, content)@ to the 'PushChannel'.
--
push :: PushChannel -> (BS.ByteString, BS.ByteString) -> MQMonad ()
push channel (msgTag, msgContent) = liftIO . sendMulti channel $ msgTag :| [msgContent]

-- | Pulls @(tag, content)@ from the 'PullChannel'.
--
pull :: PullChannel -> MQMonad (BS.ByteString, BS.ByteString)
pull channel = do
    msg' <- liftIO . receiveMulti $ channel
    processMessage msg'
    
-- | Publishes @(tag, content)@ to the 'PubChannel'.
--
pub :: PubChannel -> (BS.ByteString, BS.ByteString) -> MQMonad ()
pub channel (msgTag, msgContent) = liftIO . sendMulti channel $ msgTag :| [msgContent]

-- | Subscribes and gets @(tag, content)@ from the 'SubChannel'.
--
sub :: SubChannel -> MQMonad (BS.ByteString, BS.ByteString)
sub channel = do
    msg' <- liftIO . receiveMulti $ channel
    processMessage msg'
    
processMessage :: [BS.ByteString] -> MQMonad (BS.ByteString, BS.ByteString)
processMessage [msgTag, msgContent] = if tagIsValid msgTag
                                      then pure (msgTag, msgContent)
                                      else throwError . MQTransportError $ "tag is not valid (not consists from 5 fields)."
processMessage list = throwError . MQTransportError . printf "expected message with [header, body]; received list with %d element(s)." $ length list

tagIsValid :: BS.ByteString -> Bool
tagIsValid = (== 5) . length . BS.split delimiter




