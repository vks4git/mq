{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Transport.ByteString
  (
    push
  , pull
  , pub
  , sub
  ) where

import           Control.Monad.Except
import qualified Data.ByteString                    as BS (ByteString, unpack)
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Data.Maybe                         (fromJust, isJust)
import           Data.Text                          as T (Text, split)
import qualified System.MQ.Encoding.MessagePack     as MP (pack, unpack)
import           System.MQ.Error                    (MQError (..), errorTag,
                                                     errorTransport)
import           System.MQ.Monad                    (MQMonadS)
import           System.MQ.Protocol                 (MessageTag, delimiter)
import           System.MQ.Transport.Internal.Types (PubChannel, PullChannel,
                                                     PushChannel, SubChannel)
import           System.ZMQ4                        (receiveMulti, sendMulti)
import           Text.Printf                        (printf)

-- | Pushes @(tag, content)@ to the 'PushChannel'.
--
push :: PushChannel -> (MessageTag, BS.ByteString) -> MQMonadS s ()
push channel (msgTag, msgContent) = liftIO . sendMulti channel $ (MP.pack msgTag) :| [msgContent]

-- | Pulls @(tag, content)@ from the 'PullChannel'.
--
pull :: PullChannel -> MQMonadS s (MessageTag, BS.ByteString)
pull channel = do
    msg' <- liftIO . receiveMulti $ channel
    processMessage msg'

-- | Publishes @(tag, content)@ to the 'PubChannel'.
--
pub :: PubChannel -> (MessageTag, BS.ByteString) -> MQMonadS s ()
pub channel (msgTag, msgContent) = liftIO . sendMulti channel $ (MP.pack msgTag) :| [msgContent]

-- | Subscribes and gets @(tag, content)@ from the 'SubChannel'.
--
sub :: SubChannel -> MQMonadS s (MessageTag, BS.ByteString)
sub channel = do
    msg' <- liftIO . receiveMulti $ channel
    processMessage msg'

processMessage :: [BS.ByteString] -> MQMonadS s (MessageTag, BS.ByteString)
processMessage [msgTag, msgContent] = do
  liftIO $ print $ BS.unpack msgTag
  liftIO $ print $ BS.unpack msgContent 
  either throwError (\tag -> pure (tag, msgContent)) $ checkTag msgTag
                                     -- if tagIsValid msgTag
                                      -- then pure (msgTag, msgContent)
                                      -- else throwError . MQError errorTag $ "tag is not valid."
processMessage list = throwError . MQError errorTransport . printf "expected message with [header, body]; received list with %d element(s)." $ length list

checkTag :: BS.ByteString -> Either MQError MessageTag
checkTag bs = if isMessagePack && length delimited == 5 && head delimited `elem` msgTypes
              then Right . fromJust $ tagUnpackedM
              else Left . MQError errorTag $ "tag is not valid: " ++ show bs
  where
    tagUnpackedM :: Maybe Text
    tagUnpackedM = MP.unpack bs

    isMessagePack :: Bool
    isMessagePack = isJust tagUnpackedM

    delimited :: [Text]
    delimited = T.split ( == delimiter) $ fromJust tagUnpackedM

    msgTypes :: [Text]
    msgTypes = ["config", "result", "data", "error"]

-- tagIsValid :: Text -> Bool
-- tagIsValid bs = isMessagePack && length delimited == 5 && head delimited `elem` msgTypes
--   where
--     tagUnpackedM = MP.unpack bs
--     isMessagePack = isJust tagUnpackedM

--     delimited = BS.split delimiter $ fromJust tagUnpackedM

--     msgTypes = ["config", "result", "data", "error"]
