{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Error.Internal.Instances () where

import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..),
                                                        genericParseJSON,
                                                        genericToJSON)
import           Data.Aeson.Casing                     (aesonPrefix, snakeCase)
import qualified System.MQ.Encoding.JSON               as JSON (pack, unpack)
import           System.MQ.Error.Internal.Types        (MQError (..))
import           System.MQ.Protocol                    (MessageType (..))
import           System.MQ.Protocol.Class              (MessageLike (..),
                                                        Props (..))
import           System.MQ.Protocol.Internal.Functions (jsonEncoding)

instance ToJSON MQError where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON MQError where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance MessageLike MQError where
  props = Props "error" Error jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack
