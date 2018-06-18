{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Data.Text                         as T (intercalate, pack,
                                                         split, unpack)
import           System.MQ.Protocol.Internal.Types (Creator, Id, Message (..),
                                                    MessageTag,
                                                    MessageType (..), Spec)


-- | Build a 'MessageTag' for the given message.
-- It is consists of five fields – message_type, spec, id, pid, creator – separated by ":".
-- See doc/PROTOCOL.md#Заголовок-сообщения for more information.
--
messageTag :: Message -> MessageTag
messageTag Message{..} = T.intercalate ":" [T.pack . show $ msgType, msgSpec, msgId, msgPid, msgCreator]

-- | Filtration:
-- Use System.MQ.Protocol.Internal.Condition
-- > "config:bar:baz:ss:er" `matches` (messageType :== Config :&& messageId :== "baz")
-- > True
-- > "Config:bar:baz:ss:er" `matches` (messageType :== Config :&& messageId :== "ba")
-- > False

messageType :: MessageTag -> MessageType
messageType = read . T.unpack . head . T.split isDelimiter

messageSpec :: MessageTag -> Spec
messageSpec = (!! 1) . T.split isDelimiter

messageId :: MessageTag -> Id
messageId = (!! 2) . T.split isDelimiter

messagePid :: MessageTag -> Id
messagePid = (!! 3) . T.split isDelimiter

messageCreator :: MessageTag -> Creator
messageCreator = (!! 4) . T.split isDelimiter

delimiter :: Char
delimiter = ':'

isDelimiter :: Char -> Bool
isDelimiter = (== delimiter)
