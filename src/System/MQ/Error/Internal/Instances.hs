{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.MQ.Error.Internal.Instances () where

import           Data.Aeson                     (FromJSON (..), ToJSON (..),
                                                 genericParseJSON,
                                                 genericToJSON)
import           Data.Aeson.Casing              (aesonPrefix, snakeCase)
import           Data.MessagePack.Types.Class   (MessagePack (..))
import           System.MQ.Error.Internal.Types (MQError (..))
import           System.MQ.Protocol             (MessageType (..))
import           System.MQ.Protocol.Class       (MessageLike (..), Props (..))

instance ToJSON MQError where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON MQError where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance MessagePack MQError where
  toObject = undefined
  fromObject = undefined

instance MessageLike MQError where
  props = Props "error" Error



