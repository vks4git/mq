{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Protocol.Internal.Tag
  (
    messageTag
  , messageType
  , messageSpec
  , messageId
  , messagePid
  , messageCreator
  , delimiter
  ) where

import           Data.ByteString                   (intercalate, split)
import           Data.ByteString.Char8             as BS8 (unpack)
import           Data.Char                         (ord)
import           Data.String                       (IsString (..))
import           Data.Word                         (Word8)
import           System.MQ.Protocol.Internal.Types (Creator, Hash, Message (..),
                                                    MessageTag,
                                                    MessageType (..), Spec)


-- | Build a 'MessageTag' for the given message.
-- It is consists of five fields – message_type, spec, id, pid, creator – separated by ":".
-- See doc/PROTOCOL.md#Заголовок-сообщения for more information.
--
messageTag :: Message -> MessageTag
messageTag = intercalate ":" . ([fromString . show . msgType, fromString . msgSpec, msgId, msgPid, fromString . msgCreator] <*>) . pure

-- | Filtration:
-- Use System.MQ.Protocol.Internal.Condition
-- > "config:bar:baz:ss:er" `matches` (messageType :== Config :&& messageId :== "baz")
-- > True
-- > "Config:bar:baz:ss:er" `matches` (messageType :== Config :&& messageId :== "ba")
-- > False

messageType :: MessageTag -> MessageType
messageType = read . BS8.unpack . head . split delimiter

messageSpec :: MessageTag -> Spec
messageSpec = BS8.unpack . (!! 1) . split delimiter

messageId :: MessageTag -> Hash
messageId = (!! 2) . split delimiter

messagePid :: MessageTag -> Hash
messagePid = (!! 3) . split delimiter

messageCreator :: MessageTag -> Creator
messageCreator = BS8.unpack . (!! 4) . split delimiter

delimiter :: Word8
delimiter = fromIntegral . ord $ ':'
