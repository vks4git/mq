{-# LANGUAGE DeriveGeneric #-}

module System.MQ.Protocol.Error
  ( MQErrorData (..)
  ) where

import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..),
                                                        genericParseJSON,
                                                        genericToJSON)
import           Data.Aeson.Casing                     (aesonPrefix, snakeCase)
import           GHC.Generics                          (Generic)
import qualified System.MQ.Encoding.JSON               as JSON (pack, unpack)
import           System.MQ.Protocol.Class              (MessageLike (..),
                                                        Props (..))
import           System.MQ.Protocol.Internal.Functions (jsonEncoding)
import           System.MQ.Protocol.Internal.Types     (MessageType (..))


-- Type that represents data for error message that can be sent to queue.
newtype MQErrorData = MQErrorData { errorMessage :: String
                                  }
  deriving (Eq, Show, Generic)

instance ToJSON MQErrorData where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON MQErrorData where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance MessageLike MQErrorData where
  props = Props "error" Error jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack
