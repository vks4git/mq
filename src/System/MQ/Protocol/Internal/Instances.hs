{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.MQ.Protocol.Internal.Instances () where

import           Control.Monad                     ((>=>))
import           Data.ByteString                   as BS (ByteString)
import           Data.Map.Strict                   (Map, fromList, member, (!))
import           Data.MessagePack.Types.Class      (MessagePack (..))
import           Data.MessagePack.Types.Object     (Object)
import           Data.Text                         (Text)
import           System.MQ.Protocol.Internal.Types (Creator, Dictionary (..),
                                                    Encoding, Id,
                                                    Message (..),
                                                    MessageType (..), Spec,
                                                    Timestamp)

--------------------------------------------------------------------------------
-- Convertation for Message to and from MessagePack
--------------------------------------------------------------------------------

infix .=
(.=) :: (Ord a, MessagePack b) => a -> b -> (a, Object)
a .= b = (a, toObject b)

infix .!
(.!) :: (Monad m, MessagePack b) => Map Text Object -> Text -> m b
dict .! key | key `member` dict = fromObject $ dict ! key
            | otherwise = error $ "System.MQ.Protocol.Internal.Instances: .! :: key " ++ show key ++ " is not an element of the dictionary."

instance Dictionary Message where
  toDictionary Message{..} = fromList [ "id"         .= msgId
                                      , "pid"        .= msgPid
                                      , "creator"    .= msgCreator
                                      , "created_at" .= msgCreatedAt
                                      , "expires_at" .= msgExpiresAt
                                      , "spec"       .= msgSpec
                                      , "encoding"   .= msgEncoding
                                      , "type"       .= show msgType
                                      , "data"       .= msgData
                                      ]
  fromDictionary dict = do
    (msgId :: Id)               <- dict .! "id"
    (msgPid :: Id)              <- dict .! "pid"
    (msgCreator :: Creator)     <- dict .! "creator"
    (msgCreatedAt :: Timestamp) <- dict .! "created_at"
    (msgExpiresAt :: Timestamp) <- dict .! "expires_at"
    (msgSpec :: Spec)           <- dict .! "spec"
    (msgEncoding :: Encoding)   <- dict .! "encoding"
    (msgType :: MessageType)    <- dict .! "type"
    (msgData :: ByteString)     <- dict .! "data"
    pure Message{..}

instance MessagePack Message where
  toObject = toObject . toDictionary
  fromObject = fromObject >=> fromDictionary

instance MessagePack MessageType where
  toObject = toObject . show
  fromObject = fmap read . fromObject
