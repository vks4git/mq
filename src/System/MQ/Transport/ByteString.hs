{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Transport.ByteString
  (
    push
  , pull
  , pub
  , sub
  ) where

import           Control.Monad.Except
import qualified Data.ByteString                    as BS (ByteString)
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Data.Text                          as T (Text, split)
import           Data.Text.Encoding                 (decodeUtf8', encodeUtf8)
import           System.MQ.Error                    (MQError (..), errorTag, errorTransport)
import           System.MQ.Monad                    (MQMonadS)
import           System.MQ.Protocol                 (MessageTag, delimiter)
import           System.MQ.Transport.Internal.Types (PubChannel, PullChannel,
                                                     PushChannel, SubChannel)
import           System.ZMQ4                        (receiveMulti, sendMulti)

-- | Pushes @(tag, content)@ to the 'PushChannel'.
--
push :: PushChannel -> (MessageTag, BS.ByteString) -> MQMonadS s ()
push channel (msgTag, msgContent) = liftIO . sendMulti channel $ encodeUtf8 msgTag :| [msgContent]

-- | Pulls @(tag, content)@ from the 'PullChannel'.
--
pull :: PullChannel -> MQMonadS s (MessageTag, BS.ByteString)
pull channel = do
    msg' <- liftIO . receiveMulti $ channel
    processMessage msg'

-- | Publishes @(tag, content)@ to the 'PubChannel'.
--
pub :: PubChannel -> (MessageTag, BS.ByteString) -> MQMonadS s ()
pub channel (msgTag, msgContent) = liftIO . sendMulti channel $ encodeUtf8 msgTag :| [msgContent]

-- | Subscribes and gets @(tag, content)@ from the 'SubChannel'.
--
sub :: SubChannel -> MQMonadS s (MessageTag, BS.ByteString)
sub channel = do
    msg' <- liftIO . receiveMulti $ channel
    processMessage msg'

processMessage :: [BS.ByteString] -> MQMonadS s (MessageTag, BS.ByteString)
processMessage [msgTag, msgContent] =
  either throwError (\tag -> pure (tag, msgContent)) $ checkTag msgTag
processMessage _ = throwError $ MQError errorTransport "Expected [tag, content]"

checkTag :: BS.ByteString -> Either MQError MessageTag
checkTag bs = do
    tagUnpacked   <- either (\_ -> Left $ MQError errorTag $ "could not decode UTF8: " ++ show bs) Right (decodeUtf8' bs)
    let delimited = T.split ( == delimiter) tagUnpacked

    if length delimited == 5 && head delimited `elem` msgTypes
              then Right tagUnpacked
              else Left . MQError errorTag $ "tag is not valid: " ++ show bs
  where
    msgTypes :: [Text]
    msgTypes = ["config", "result", "data", "error"]
